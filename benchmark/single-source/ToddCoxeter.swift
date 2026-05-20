//
// This file implements an algorithm to construct the Cayley graph for a
// finite monoid. Specifically, this implements the "modified Felsch
// strategy" for congruence enumeration, described in this paper:
//
// "The Todd–Coxeter algorithm for semigroups and monoids"
// https://link.springer.com/article/10.1007/s00233-024-10431-z
//
// A few things you would need for a "real" implementation are missing:
//
// 1) There is no garbage collection for nodes in the graph, so when two nodes
//    are merged, the dead node is not reused. Fixing this would decrease
//    memory pressure.
// 2) Other strategies, such as the HLT strategy, could be implemented, since
//    alternating Flesch with HLT gives better performance on some workloads.
// 3) The constructed word graph is not standardized, and the only thing this
//    implementation does with it is count the number of elements.
//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "ToddCoxeter", runFunction: run_ToddCoxeter, tags: [.miniapplication])
]

func run(rules: [[Word]], maxNodes: Int, maxLinks: Int)
    throws(CongruenceError) -> Int {
  let alphabet = 2
  let rules = rules.map { Rule(lhs: $0[0], rhs: $0[1]) }
  let p = Presentation(alphabet: alphabet, rules: rules)

  var c = Congruence(presentation: p, maxNodes: maxNodes, maxLinks: maxLinks)
  return try c.enumerate()
}

let problems: [([[Word]], Int, Int, Int)] = [
  // <a, b | aaa=1, abbbba=b> has 1083 elements. This takes about half a millisecond
  // on an Apple MacBook Pro with M4 chip. This runs by default.
  ([[[0, 0, 0],    []], [[0, 1, 1, 1, 1, 0],    [1]]], 2000,     3000,     1083),

  // <a, b | aaa=1, ababbba=b> has 10,587 elements. This takes about 9 milliseconds
  // on an Apple MacBook Pro with M4 chip.
  ([[[0, 0, 0],    []], [[0, 1, 0, 1, 1, 1, 0], [1]]], 30000,    30000,    10587),

  // <a, b | aaaa=1, ababba=b> has 2,361,964 elements. This takes about 22 seconds
  // on an Apple MacBook Pro with M4 chip.
  ([[[0, 0, 0, 0], []], [[0, 1, 0, 1, 1, 0],    [1]]], 40000000, 40000000, 2361964),
]

// Change 0 to 1 or 2 to run the longer workloads.
let problem = problems[0]

func run_ToddCoxeter(_ n: Int) {
  let (rules, maxNodes, maxLinks, expected) = problem
  for _ in 0 ..< n {
    let result = try! run(rules: rules, maxNodes: maxNodes, maxLinks: maxLinks)
    precondition(result == expected)
  }
}

typealias Symbol = UInt8

typealias Word = [Symbol]

struct Rule {
  var lhs: Word
  var rhs: Word
}

struct Presentation {
  var alphabet: Int
  var rules: [Rule]
}

/// Utility function. This actually only gets called with an input value of '2'
/// so it doesn't need to be optimized.
func nextPowerOfTwo(_ x: Int) -> Int {
  precondition(x > 0)
  var y = 1
  while y < x {
    y *= 2
  }
  return y
}

enum CongruenceError: Error {
  case tooManyNodes
  case tooManyLinks
}

/// A tree:
/// - The nodes in the tree correspond to the factors of the words that appear
///   in the monoid's defining relations.
/// - For each letter 'a' in the alphabet, there is an edge joniing each node
///   'x' with the node 'ax'.
/// - Each node then stores the index of each defining relation where one of the
///   two sides has the node's word as a prefix.
/// - The nodes are stored in a flat array, with the root appearing at index 0.
struct FelschTree {
  typealias Node = Int16

  static let none: Node = -1

  var alphabet: Int

  /// Mapping from node index to list of defining relations that have this node's
  /// word as a prefix.
  var values: [[Int]] = []

  /// An array storing the mth child of the nth node at index 'n * alphabet + m'.
  var children: [Node] = []

  init(alphabet: Int) {
    self.alphabet = alphabet

    let root = createNode()
    precondition(root == 0)
  }

  func child(_ node: Node, _ s: Symbol) -> Int {
    return Int(node) * alphabet + Int(s)
  }

  func follow(_ node: Node, _ s: Symbol) -> Node {
    return children[child(node, s)]
  }

  mutating func setValue(_ node: Node, _ value: [Int]) {
    precondition(values[Int(node)].isEmpty)
    values[Int(node)] = value
  }

  mutating func createNode() -> Node {
    let node = Node(values.count)
    values.append([])
    children.append(contentsOf: Array(repeating: Self.none, count: alphabet))
    return node
  }

  mutating func insert(word: Word, from: Int, value: [Int]) {
    precondition(from > 0)

    var from = from
    var j: Node = 0

    while from > 0 {
      let s = word[from - 1]
      var next = children[child(j, s)]
      if next == Self.none {
        next = createNode()
        children[child(j, s)] = next
      }
      j = next
      from -= 1
    }

    setValue(j, value)
  }

  mutating func insert(word: Word, value: [Int]) {
    insert(word: word, from: word.count, value: value)
  }
}

extension Presentation {
  func buildFelschTree() -> FelschTree {
    // Collect unique factors of the defining relations.
    var factors: [Word] = []
    var unique: Set<Word> = []

    for rule in rules {
      for side in [rule.lhs, rule.rhs] {
        if side.isEmpty {
          continue
        }
        for i in 0 ... side.count {
          for j in i ..< side.count {
            let factor = Word(side[i ... j])
            if unique.insert(factor).inserted {
              factors.append(factor)
            }
          }
        }
      }
    }

    // Build the tree.
    var result = FelschTree(alphabet: alphabet)
    for factor in factors {
      let value = rules.indices.filter {
        return rules[$0].lhs.starts(with: factor) ||
               rules[$0].rhs.starts(with: factor)
      }

      if !value.isEmpty {
        // Add a node for each factor that is a prefix of at least one side of
        // at least one defining relation.
        result.insert(word: factor, value: value)
      }
    }

    return result
  }
}

struct Congruence: ~Copyable {
  typealias Node = Int32
  typealias Link = Int32

  static let none: Int32 = -1

  let p: Presentation

  // Base-2 logarithm of the alphabet rounded up to the next power of two.
  // Shifting by ashift converts between indices into the 'values' and
  // 'parents' arrays below.
  let ashift: Int

  let tree: FelschTree

  // Counters.
  var iterations = 0
  var merged = 0
  var checked = 0
  var nodes = 0

  // First node we have not yet performed "TC1" on.
  var cursor: Node = 0

  // The word graph is represented as an array of nodes. Nodes are identified
  // by their index in this array. Nodes are sized to the alphabet, rounded up
  // to the next power of two, so the index is always a multiple of the node
  // size. The mth child of the nth node is stored at index 'm + n'. This is the
  // destination of the edge labeled by the nth letter of the alphabet.
  var values: [Node]

  // Union-find map, the parent of the nth node is stored at index 'n >> ashift'.
  var parents: [Node]

  // Next word graph nodes that is unused.
  var freeNode: Node = 0

  // For each node, we also store a linked list of predecessors. The head of the
  // mth list of predecessors for the nth node is stored at index 'm + n' in this
  // array. This list points at all nodes whose mth child is n.
  var heads: [Link]

  // A list of linked list nodes
  var links: [(Node, Link)]

  // Next list node that is unused.
  var freeLink: Link = 0

  // When we record an edge from 'x' to 'y' labeled by 's', we add (x, s) to
  // this list.
  var recentList: [(Node, Symbol)] = []

  // Work list for steps "PD1" and "PD2" in the modified Felsch strategy.
  var workList: [(Node, FelschTree.Node)] = []

  // Work list for step "TC3".
  var mergeList: [(Node, Node)] = []

  init(presentation: Presentation, maxNodes: Int, maxLinks: Int) {
    self.p = presentation
    ashift = nextPowerOfTwo(p.alphabet)
    tree = p.buildFelschTree()
    values = Array(repeating: Self.none, count: maxNodes << ashift)
    parents = Array(repeating: Self.none, count: maxNodes)
    heads = Array(repeating: Self.none, count: maxNodes << ashift)
    links = Array(repeating: (Self.none, Self.none),
                  count: maxLinks << ashift)
  }

  mutating func find(_ node: Node) -> Node {
    let index = Int(node) >> ashift
    let parent = parents[index]
    if node != parent {
      let canonical = find(parent)
      if canonical != parent {
        parents[index] = canonical
      }
      return canonical
    }

    return node
  }

  /// Allocate a graph node.
  mutating func createNode() throws(CongruenceError) -> Node {
    if freeNode == values.count {
      throw CongruenceError.tooManyNodes
    }

    let node = freeNode
    freeNode += (1 << ashift)

    nodes += 1
    parents[Int(node) >> ashift] = node

    return node
  }

  /// Allocate a linked list node.
  mutating func createLink() throws(CongruenceError) -> Link {
    if freeLink == links.count {
      throw CongruenceError.tooManyLinks
    }

    let link = freeLink
    freeLink += 1
    return link
  }

  /// Record a predecessor of a node in the reverse index.
  mutating func recordLink(from: Node, label: Symbol, to: Node)
      throws(CongruenceError) {
    let link = try createLink()
    let index = Int(to) + Int(label)
    links[Int(link)] = (from, heads[index])
    heads[index] = link
  }

  func rawEdge(from: Node, label: Symbol) -> Node {
    let index = Int(from) + Int(label)
    return values[index]
  }

  func edge(from: Node, label: Symbol) -> Node {
    let next = rawEdge(from: from, label: label)
    if next == Self.none {
      return next
    }
    precondition(parents[Int(next) >> ashift] == next)
    return next
  }

  func follow(from: Node, word: some Sequence<Symbol>) -> Node {
    var node = from
    for s in word {
      node = edge(from: node, label: s)
      if node == Self.none {
        break
      }
    }
    return node
  }

  mutating func setEdge(from: Node, label: Symbol, to: Node,
                        overwrite: Bool=false) throws(CongruenceError) {
    precondition(parents[Int(from) >> ashift] == from)
    precondition(parents[Int(to) >> ashift] == to)

    let i = Int(from) + Int(label)
    precondition(overwrite != (values[i] == Self.none))

    values[i] = to

    try recordLink(from: from, label: label, to: to)

    recentList.append((from, label))
  }

  /// TC1: Define a new node.
  mutating func expand(from: Node) throws(CongruenceError) -> Bool {
    var changed = false

    for s in 0 ..< Symbol(p.alphabet) {
      let next = edge(from: from, label: s)
      if next == Self.none {
        let to = try createNode()
        try setEdge(from: from, label: s, to: to)
        changed = true
      }
    }

    return changed
  }

  /// TC2: Follow paths defined by a relation.
  mutating func relation(from: Node, rule: Rule) throws(CongruenceError) {
    checked += 1

    if !rule.lhs.isEmpty && !rule.rhs.isEmpty {
      let lhsP = rule.lhs.dropLast()
      let xP = follow(from: from, word: lhsP)
      if xP == Self.none {
        return
      }
      let lhsS = rule.lhs.last!
      let x = edge(from: xP, label: lhsS)

      let rhsP = rule.rhs.dropLast()
      let yP = follow(from: from, word: rhsP)
      if yP == Self.none {
        return
      }
      let rhsS = rule.rhs.last!
      let y = edge(from: yP, label: rhsS)

      if x == y {
        return
      } else if x == Self.none {
        precondition(y != Self.none)
        try setEdge(from: xP, label: lhsS, to: y)
        return
      } else if y == Self.none {
        precondition(x != Self.none)
        try setEdge(from: yP, label: rhsS, to: x)
        return
      } else {
        precondition(x != y)
        mergeList.append((x, y))
        return
      }
    } else if !rule.lhs.isEmpty && rule.rhs.isEmpty {
      let lhsP = rule.lhs.dropLast()

      let xP = follow(from: from, word: lhsP)
      if xP == Self.none {
        return
      }

      let lhsS = rule.lhs.last!
      let x = edge(from: xP, label: lhsS)

      if x == from {
        return
      } else if x == Self.none {
        try setEdge(from: xP, label: lhsS, to: from)
        return
      } else {
        mergeList.append((x, from))
        return
      }
    } else if rule.lhs.isEmpty && !rule.rhs.isEmpty {
      let rhsP = rule.rhs.dropLast()
      let yP = follow(from: from, word: rhsP)
      if yP == Self.none {
        return
      }

      let rhsS = rule.rhs.last!
      let y = edge(from: yP, label: rhsS)

      if y == from {
       return
      } else if y == Self.none {
        try setEdge(from: yP, label: rhsS, to: from)
        return
      } else {
        mergeList.append((y, from))
        return
      }
    }

    /// The if statements above should be exhaustive.
    fatalError()
  }

  /// Reverse index walk using Felsch tree. Given a recently-added edge,
  /// walk all nodes that possibly changed as a result, and re-apply "TC2"
  /// to each node.
  mutating func closure(from: Node, label: Symbol) throws(CongruenceError) {
    let node = tree.follow(0, label)
    precondition(node != FelschTree.none)

    precondition(workList.isEmpty)
    workList.append((from, node))

    while !workList.isEmpty {
      let (from, node) = workList.removeLast()
      precondition(parents[Int(from) >> ashift] == from)

      for i in tree.values[Int(node)] {
        try relation(from: from, rule: p.rules[i])
      }

      for s in 0 ..< Symbol(p.alphabet) {
        let child = tree.follow(node, s)
        if child != FelschTree.none {
          var link = heads[Int(from) + Int(s)]
          while link != Self.none {
            let pred = find(links[Int(link)].0)
            workList.append((pred, child))
            link = links[Int(link)].1
          }
        }
      }
    }
  }

  /// Step "PD2" in modified Felsch strategy.
  mutating func closure() throws(CongruenceError) {
    while !recentList.isEmpty {
      let (node, s) = recentList.removeLast()
      if parents[Int(node) >> ashift] == node {
        try closure(from: node, label: s)
      }

      try merge()
    }
  }

  /// TC3: Process coincidences or a determination.
  mutating func merge() throws(CongruenceError) {
    func mergeLinks(_ x: Node, _ y: Node) throws(CongruenceError) {
      for s in 0 ..< Symbol(p.alphabet) {
        let index = Int(x) + Int(s)
        var link = heads[index]
        while link != Self.none {
          let ref = links[Int(link)].0
          if parents[Int(ref) >> ashift] == ref {
            let next = rawEdge(from: ref, label: s)
            if next != y {
              try setEdge(from: ref, label: s, to: y, overwrite: true)
            }
          }
          link = links[Int(link)].1
        }
      }
    }

    func union(_ x: Node, _ y: Node) throws(CongruenceError) {
      var x = find(x)
      var y = find(y)
      if x == y {
        return
      } else if x < y {
        (x, y) = (y, x)
      }

      parents[Int(x) >> ashift] = y

      try mergeLinks(x, y)

      merged += 1

      for i in 0 ..< Symbol(p.alphabet) {
        let xx = edge(from: x, label: i)
        let yy = edge(from: y, label: i)

        if xx != yy && xx != Self.none && yy != Self.none {
          mergeList.append((xx, yy))
        } else if yy == Self.none && xx != Self.none {
          try setEdge(from: y, label: i, to: xx)
        }
      }
    }

    while !mergeList.isEmpty {
      let (x, y) = mergeList.removeLast()
      try union(x, y)
    }
  }

  /// Run the congruence enumeration.
  mutating func enumerate() throws(CongruenceError) -> Int {
    let root = try! createNode()
    precondition(root == 0)

    repeat {
      try closure()

      while Int(cursor) < values.count {
        if parents[Int(cursor) >> ashift] == cursor {
          if try expand(from: cursor) {
            break
          }
        }

        cursor += 1
      }

      precondition(mergeList.isEmpty)
    } while !recentList.isEmpty

    return nodes - merged
  }
}
