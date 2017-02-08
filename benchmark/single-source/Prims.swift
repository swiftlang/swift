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
  var hashValue: Int {
    get {
      return start.hashValue ^ end.hashValue
    }
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

@inline(never)
public func run_Prims(_ N: Int) {
  for _ in 1...5*N {
    let nodes : [Int] = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
      29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
      45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
      61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
      77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,
      93, 94, 95, 96, 97, 98, 99 ]

    // Prim's algorithm is designed for undirected graphs.
    // Due to that, in our set all the edges are paired, i.e. for any
    // edge (start, end, C) there is also an edge (end, start, C).
    let edges : [(Int, Int, Double)] = [
      (26, 47, 921),
      (20, 25, 971),
      (92, 59, 250),
      (33, 55, 1391),
      (78, 39, 313),
      (7, 25, 637),
      (18, 19, 1817),
      (33, 41, 993),
      (64, 41, 926),
      (88, 86, 574),
      (93, 15, 1462),
      (86, 33, 1649),
      (37, 35, 841),
      (98, 51, 1160),
      (15, 30, 1125),
      (65, 78, 1052),
      (58, 12, 1273),
      (12, 17, 285),
      (45, 61, 1608),
      (75, 53, 545),
      (99, 48, 410),
      (97, 0, 1303),
      (48, 17, 1807),
      (1, 54, 1491),
      (15, 34, 807),
      (94, 98, 646),
      (12, 69, 136),
      (65, 11, 983),
      (63, 83, 1604),
      (78, 89, 1828),
      (61, 63, 845),
      (18, 36, 1626),
      (68, 52, 1324),
      (14, 50, 690),
      (3, 11, 943),
      (21, 68, 914),
      (19, 44, 1762),
      (85, 80, 270),
      (59, 92, 250),
      (86, 84, 1431),
      (19, 18, 1817),
      (52, 68, 1324),
      (16, 29, 1108),
      (36, 80, 395),
      (67, 18, 803),
      (63, 88, 1717),
      (68, 21, 914),
      (75, 82, 306),
      (49, 82, 1292),
      (73, 45, 1876),
      (89, 82, 409),
      (45, 47, 272),
      (22, 83, 597),
      (61, 12, 1791),
      (44, 68, 1229),
      (50, 51, 917),
      (14, 53, 355),
      (77, 41, 138),
      (54, 21, 1870),
      (93, 70, 1582),
      (76, 2, 1658),
      (83, 73, 1162),
      (6, 1, 482),
      (11, 65, 983),
      (81, 90, 1024),
      (19, 1, 970),
      (8, 58, 1131),
      (60, 42, 477),
      (86, 29, 258),
      (69, 59, 903),
      (34, 15, 807),
      (37, 2, 1451),
      (7, 73, 754),
      (47, 86, 184),
      (67, 17, 449),
      (18, 67, 803),
      (25, 4, 595),
      (3, 31, 1337),
      (64, 31, 1928),
      (9, 43, 237),
      (83, 63, 1604),
      (47, 45, 272),
      (86, 88, 574),
      (87, 74, 934),
      (98, 94, 646),
      (20, 1, 642),
      (26, 92, 1344),
      (18, 17, 565),
      (47, 11, 595),
      (10, 59, 1558),
      (2, 76, 1658),
      (77, 74, 1277),
      (42, 60, 477),
      (80, 36, 395),
      (35, 23, 589),
      (50, 37, 203),
      (6, 96, 481),
      (78, 65, 1052),
      (1, 52, 127),
      (65, 23, 1932),
      (46, 51, 213),
      (59, 89, 89),
      (15, 93, 1462),
      (69, 3, 1305),
      (17, 37, 1177),
      (30, 3, 193),
      (9, 15, 818),
      (75, 95, 977),
      (86, 47, 184),
      (10, 12, 1736),
      (80, 27, 1010),
      (12, 10, 1736),
      (86, 1, 1958),
      (60, 12, 1240),
      (43, 71, 683),
      (91, 65, 1519),
      (33, 86, 1649),
      (62, 26, 1773),
      (1, 13, 1187),
      (2, 10, 1018),
      (91, 29, 351),
      (69, 12, 136),
      (43, 9, 237),
      (29, 86, 258),
      (17, 48, 1807),
      (31, 64, 1928),
      (68, 61, 1936),
      (76, 38, 1724),
      (1, 6, 482),
      (53, 14, 355),
      (51, 50, 917),
      (54, 13, 815),
      (19, 29, 883),
      (35, 87, 974),
      (70, 96, 511),
      (23, 35, 589),
      (39, 69, 1588),
      (93, 73, 1093),
      (13, 73, 435),
      (5, 60, 1619),
      (42, 41, 1523),
      (66, 58, 1596),
      (1, 67, 431),
      (17, 67, 449),
      (30, 95, 906),
      (71, 43, 683),
      (5, 87, 190),
      (12, 78, 891),
      (30, 97, 402),
      (28, 17, 1131),
      (7, 97, 1356),
      (58, 66, 1596),
      (20, 37, 1294),
      (73, 76, 514),
      (54, 8, 613),
      (68, 35, 1252),
      (92, 32, 701),
      (3, 90, 652),
      (99, 46, 1576),
      (13, 54, 815),
      (20, 87, 1390),
      (36, 18, 1626),
      (51, 26, 1146),
      (2, 23, 581),
      (29, 7, 1558),
      (88, 59, 173),
      (17, 1, 1071),
      (37, 49, 1011),
      (18, 6, 696),
      (88, 33, 225),
      (58, 38, 802),
      (87, 50, 1744),
      (29, 91, 351),
      (6, 71, 1053),
      (45, 24, 1720),
      (65, 91, 1519),
      (37, 50, 203),
      (11, 3, 943),
      (72, 65, 1330),
      (45, 50, 339),
      (25, 20, 971),
      (15, 9, 818),
      (14, 54, 1353),
      (69, 95, 393),
      (8, 66, 1213),
      (52, 2, 1608),
      (50, 14, 690),
      (50, 45, 339),
      (1, 37, 1273),
      (45, 93, 1650),
      (39, 78, 313),
      (1, 86, 1958),
      (17, 28, 1131),
      (35, 33, 1667),
      (23, 2, 581),
      (51, 66, 245),
      (17, 54, 924),
      (41, 49, 1629),
      (60, 5, 1619),
      (56, 93, 1110),
      (96, 13, 461),
      (25, 7, 637),
      (11, 69, 370),
      (90, 3, 652),
      (39, 71, 1485),
      (65, 51, 1529),
      (20, 6, 1414),
      (80, 85, 270),
      (73, 83, 1162),
      (0, 97, 1303),
      (13, 33, 826),
      (29, 71, 1788),
      (33, 12, 461),
      (12, 58, 1273),
      (69, 39, 1588),
      (67, 75, 1504),
      (87, 20, 1390),
      (88, 97, 526),
      (33, 88, 225),
      (95, 69, 393),
      (2, 52, 1608),
      (5, 25, 719),
      (34, 78, 510),
      (53, 99, 1074),
      (33, 35, 1667),
      (57, 30, 361),
      (87, 58, 1574),
      (13, 90, 1030),
      (79, 74, 91),
      (4, 86, 1107),
      (64, 94, 1609),
      (11, 12, 167),
      (30, 45, 272),
      (47, 91, 561),
      (37, 17, 1177),
      (77, 49, 883),
      (88, 23, 1747),
      (70, 80, 995),
      (62, 77, 907),
      (18, 4, 371),
      (73, 93, 1093),
      (11, 47, 595),
      (44, 23, 1990),
      (20, 0, 512),
      (3, 69, 1305),
      (82, 3, 1815),
      (20, 88, 368),
      (44, 45, 364),
      (26, 51, 1146),
      (7, 65, 349),
      (71, 39, 1485),
      (56, 88, 1954),
      (94, 69, 1397),
      (12, 28, 544),
      (95, 75, 977),
      (32, 90, 789),
      (53, 1, 772),
      (54, 14, 1353),
      (49, 77, 883),
      (92, 26, 1344),
      (17, 18, 565),
      (97, 88, 526),
      (48, 80, 1203),
      (90, 32, 789),
      (71, 6, 1053),
      (87, 35, 974),
      (55, 90, 1808),
      (12, 61, 1791),
      (1, 96, 328),
      (63, 10, 1681),
      (76, 34, 871),
      (41, 64, 926),
      (42, 97, 482),
      (25, 5, 719),
      (23, 65, 1932),
      (54, 1, 1491),
      (28, 12, 544),
      (89, 10, 108),
      (27, 33, 143),
      (67, 1, 431),
      (32, 45, 52),
      (79, 33, 1871),
      (6, 55, 717),
      (10, 58, 459),
      (67, 39, 393),
      (10, 4, 1808),
      (96, 6, 481),
      (1, 19, 970),
      (97, 7, 1356),
      (29, 16, 1108),
      (1, 53, 772),
      (30, 15, 1125),
      (4, 6, 634),
      (6, 20, 1414),
      (88, 56, 1954),
      (87, 64, 1950),
      (34, 76, 871),
      (17, 12, 285),
      (55, 59, 321),
      (61, 68, 1936),
      (50, 87, 1744),
      (84, 44, 952),
      (41, 33, 993),
      (59, 18, 1352),
      (33, 27, 143),
      (38, 32, 1210),
      (55, 70, 1264),
      (38, 58, 802),
      (1, 20, 642),
      (73, 13, 435),
      (80, 48, 1203),
      (94, 64, 1609),
      (38, 28, 414),
      (73, 23, 1113),
      (78, 12, 891),
      (26, 62, 1773),
      (87, 43, 579),
      (53, 6, 95),
      (59, 95, 285),
      (88, 63, 1717),
      (17, 5, 633),
      (66, 8, 1213),
      (41, 42, 1523),
      (83, 22, 597),
      (95, 30, 906),
      (51, 65, 1529),
      (17, 49, 1727),
      (64, 87, 1950),
      (86, 4, 1107),
      (37, 98, 1102),
      (32, 92, 701),
      (60, 94, 198),
      (73, 98, 1749),
      (4, 18, 371),
      (96, 70, 511),
      (7, 29, 1558),
      (35, 37, 841),
      (27, 64, 384),
      (12, 33, 461),
      (36, 38, 529),
      (69, 16, 1183),
      (91, 47, 561),
      (85, 29, 1676),
      (3, 82, 1815),
      (69, 58, 1579),
      (93, 45, 1650),
      (97, 42, 482),
      (37, 1, 1273),
      (61, 4, 543),
      (96, 1, 328),
      (26, 0, 1993),
      (70, 64, 878),
      (3, 30, 193),
      (58, 69, 1579),
      (4, 25, 595),
      (31, 3, 1337),
      (55, 6, 717),
      (39, 67, 393),
      (78, 34, 510),
      (75, 67, 1504),
      (6, 53, 95),
      (51, 79, 175),
      (28, 91, 1040),
      (89, 78, 1828),
      (74, 93, 1587),
      (45, 32, 52),
      (10, 2, 1018),
      (49, 37, 1011),
      (63, 61, 845),
      (0, 20, 512),
      (1, 17, 1071),
      (99, 53, 1074),
      (37, 20, 1294),
      (10, 89, 108),
      (33, 92, 946),
      (23, 73, 1113),
      (23, 88, 1747),
      (49, 17, 1727),
      (88, 20, 368),
      (21, 54, 1870),
      (70, 93, 1582),
      (59, 88, 173),
      (32, 38, 1210),
      (89, 59, 89),
      (23, 44, 1990),
      (38, 76, 1724),
      (30, 57, 361),
      (94, 60, 198),
      (59, 10, 1558),
      (55, 64, 1996),
      (12, 11, 167),
      (36, 24, 1801),
      (97, 30, 402),
      (52, 1, 127),
      (58, 87, 1574),
      (54, 17, 924),
      (93, 74, 1587),
      (24, 36, 1801),
      (2, 37, 1451),
      (91, 28, 1040),
      (59, 55, 321),
      (69, 11, 370),
      (8, 54, 613),
      (29, 85, 1676),
      (44, 19, 1762),
      (74, 79, 91),
      (93, 56, 1110),
      (58, 10, 459),
      (41, 50, 1559),
      (66, 51, 245),
      (80, 19, 1838),
      (33, 79, 1871),
      (76, 73, 514),
      (98, 37, 1102),
      (45, 44, 364),
      (16, 69, 1183),
      (49, 41, 1629),
      (19, 80, 1838),
      (71, 57, 500),
      (6, 4, 634),
      (64, 27, 384),
      (84, 86, 1431),
      (5, 17, 633),
      (96, 88, 334),
      (87, 5, 190),
      (70, 21, 1619),
      (55, 33, 1391),
      (10, 63, 1681),
      (11, 62, 1339),
      (33, 13, 826),
      (64, 70, 878),
      (65, 72, 1330),
      (70, 55, 1264),
      (64, 55, 1996),
      (50, 41, 1559),
      (46, 99, 1576),
      (88, 96, 334),
      (51, 20, 868),
      (73, 7, 754),
      (80, 70, 995),
      (44, 84, 952),
      (29, 19, 883),
      (59, 69, 903),
      (57, 53, 1575),
      (90, 13, 1030),
      (28, 38, 414),
      (12, 60, 1240),
      (85, 58, 573),
      (90, 55, 1808),
      (4, 10, 1808),
      (68, 44, 1229),
      (92, 33, 946),
      (90, 81, 1024),
      (53, 75, 545),
      (45, 30, 272),
      (41, 77, 138),
      (21, 70, 1619),
      (45, 73, 1876),
      (35, 68, 1252),
      (13, 96, 461),
      (53, 57, 1575),
      (82, 89, 409),
      (28, 61, 449),
      (58, 61, 78),
      (27, 80, 1010),
      (61, 58, 78),
      (38, 36, 529),
      (80, 30, 397),
      (18, 59, 1352),
      (62, 11, 1339),
      (95, 59, 285),
      (51, 98, 1160),
      (6, 18, 696),
      (30, 80, 397),
      (69, 94, 1397),
      (58, 85, 573),
      (48, 99, 410),
      (51, 46, 213),
      (57, 71, 500),
      (91, 30, 104),
      (65, 7, 349),
      (79, 51, 175),
      (47, 26, 921),
      (4, 61, 543),
      (98, 73, 1749),
      (74, 77, 1277),
      (61, 28, 449),
      (58, 8, 1131),
      (61, 45, 1608),
      (74, 87, 934),
      (71, 29, 1788),
      (30, 91, 104),
      (13, 1, 1187),
      (0, 26, 1993),
      (82, 49, 1292),
      (43, 87, 579),
      (24, 45, 1720),
      (20, 51, 868),
      (77, 62, 907),
      (82, 75, 306),
    ]

    // Prepare graph and edge->cost map
    var graph = Array<GraphNode>()
    for n in nodes {
      graph.append(GraphNode(i: n))
    }
    var map = Dictionary<Edge, Double>()
    for tup in edges {
      map[Edge(start: tup.0, end: tup.1)] = tup.2
      graph[tup.0].adjList.append(tup.1)
    }

    // Find spanning tree
    let treeEdges = Prims(graph, { (start: Int, end: Int) in
        return map[Edge(start: start, end: end)]!
    })

    // Compute its cost in order to check results
    var cost = 0.0
    for i in 1..<treeEdges.count {
      if let n = treeEdges[i] { cost += map[Edge(start: n, end: i)]! }
    }
    CheckResults(Int(cost) == 49324,
                 "Incorrect results in Prims: \(Int(cost)) != 49324.")
  }
}
