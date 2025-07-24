struct Trie {
  typealias Node = Int16

  var values: [Node] = []
  var children: [Node] = []
  var freeList: [Node] = []

  let emptyNode: [Node]
  
  init(alphabet: Int) {
    self.emptyNode = Array(repeating: -1, count: alphabet)
    _ = try! createNode()  // The root node
  }

  mutating func createNode() throws(RewritingError) -> Node {
    if !freeList.isEmpty {
      let result = Int(freeList.removeLast())
      values.replaceSubrange(result ..< result + emptyNode.count, with: emptyNode)
      children.replaceSubrange(result ..< result + emptyNode.count, with: emptyNode)
      return Node(result)
    }

    let result = values.count
    if result + emptyNode.count >= 32000 {
      throw RewritingError.tooManyNodes
    }
    values.append(contentsOf: emptyNode)
    children.append(contentsOf: emptyNode)
    precondition(values.count == children.count)
    precondition(values.count % emptyNode.count == 0)
    return Node(result)
  }

  mutating func reclaimNode(_ node: Node) {
    freeList.append(node)
  }

  mutating func insert(_ key: Word, _ value: Int) throws(RewritingError) {
    var node = 0
    for i in 0 ..< key.count - 1 {
      let s = key[i]
      if children[node + Int(s)] == -1 {
        children[node + Int(s)] = try createNode()
      }
      node = Int(children[node + Int(s)])
    }
    values[node + Int(key.last!)] = Node(value)
  }

  func lookup(_ key: Word, _ i: Int) -> Int? {
    var node = 0
    for s in key[i ..< key.count] {
      let n = Int(values[node + Int(s)])
      if n != -1 { return n }
      node = Int(children[node + Int(s)])
      if node == -1 { return nil }
    }
    return nil
  }

  // Visits all keys that are equal to a prefix of key[i ...], as well as
  // all keys whose prefix is equal to key[i ...].
  func visitOverlaps(_ key: Word, _ from: Int, callback: (Int) -> ()) {
    var node = 0
    for s in key[from...] {
      let n = Int(values[node + Int(s)])
      if n != -1 { callback(n) }
      node = Int(children[node + Int(s)])
      if node == -1 { return }
    }

    if node == 0 { return }

    var stack: [Int] = [node]

    repeat {
      let node = stack.removeLast()

      for s in (0 ..< emptyNode.count) {
        let n = Int(values[node + s])
        if n != -1 { callback(n) }
        let child = Int(children[node + s])
        if child != -1 { stack.append(child) }
      }
    } while !stack.isEmpty
  }

  func isEmptyNode(_ node: Int) -> Bool {
    for i in 0 ..< emptyNode.count {
      if values[node + i] != -1 ||
         children[node + i] != -1 {
        return false
      }
    }

    return true
  }

  mutating func remove(_ key: Word, _ value: Int) {
    var node = 0
    var stack: [Int] = []  // path to current node from root

    for i in 0 ..< key.count - 1 {
      let s = key[i]
      precondition(children[node + Int(s)] != -1)

      stack.append(node)
      node = Int(children[node + Int(s)])
    }

    let j = node + Int(key.last!)
    precondition(values[j] == value)
    values[j] = -1

    // Remove any newly-empty nodes, up to the root.
    repeat {
      if !isEmptyNode(node) { return }

      reclaimNode(Node(node))

      let parent = stack.removeLast()

      var sawThis = false
      for i in 0 ..< emptyNode.count {
        if Int(children[parent + i]) == node {
          children[parent + i] = -1
          sawThis = true
          break
        }
      }
      precondition(sawThis)

      node = parent
    } while !stack.isEmpty
  }
}
