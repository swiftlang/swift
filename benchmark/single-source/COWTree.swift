// COWTree benchmark
//
// Description: Copy-On-Write behaviour for a struct
// Source: https://gist.github.com/airspeedswift/71f15d1eb866be9e5ac7

import TestsUtils

public var COWTree = BenchmarkInfo(
  name: "COWTree",
  runFunction: run_COWTree,
  tags: [.validation, .abstraction, .String],
  legacyFactor: 20
)

@inline(never)
public func run_COWTree(_ N: Int) {
  var tree1 = Tree<String>()
  var tree2 = Tree<String>()
  var tree3 = Tree<String>()

  for _ in 1...50*N {
    tree1 = Tree<String>()
    tree1.insert("Emily")
    tree2 = tree1
    tree1.insert("Gordon")
    tree3 = tree2
    tree3.insert("Spencer")

    if !checkRef(tree1, tree2, tree3) {
      break
    }
  }

  CheckResults(checkRef(tree1, tree2, tree3))
}

@inline(never)
func checkRef(_ t1: Tree<String>, _ t2: Tree<String>,
  _ t3: Tree<String>) -> Bool {
  if !(t1.contains("Emily") && t1.contains("Gordon") &&
    !t1.contains("Spencer")) {
    return false
  }
  if !(t2.contains("Emily") && !t2.contains("Gordon") &&
    !t2.contains("Spencer")) {
    return false
  }
  if !(t3.contains("Emily") && !t3.contains("Gordon") &&
    t3.contains("Spencer")) {
    return false
  }
  return true
}

// ideally we would define this inside the tree struct
private class _Node<T: Comparable> {
  var _value: T
  var _left: _Node<T>? = nil
  var _right: _Node<T>? = nil

  init(value: T) { _value = value }
}

public struct Tree<T: Comparable> {
  // this makes things a bit more pleasant later
  fileprivate typealias Node = _Node<T>

  fileprivate var _root: Node? = nil

  public init() { }

  // constructor from a sequence
  public init<S: Sequence>(_ seq: S) where S.Element == T {
    var g = seq.makeIterator()
    while let x = g.next() {
      self.insert(x)
    }
  }

  private mutating func ensureUnique() {
    if !isKnownUniquelyReferenced(&_root) {
      // inefficiently...
      self = Tree<T>(self)
    }
  }

  public mutating func insert(_ value: T) {
    ensureUnique()
    _root = insert(_root, value)
  }

  private mutating func insert(_ node: Node?, _ value: T) -> Node? {
    switch node {
    case .none:
      return Node(value: value)
    case let .some(node) where value < node._value:
      node._left = insert(node._left, value)
      return node
    case let .some(node):
      node._right = insert(node._right, value)
      return node
    }
  }

  public func contains(_ value: T) -> Bool {
    return contains(_root, value)
  }

  private func contains(_ node: Node?, _ value: T) -> Bool {
    switch node {
    case .none:
      return false
    case let .some(node) where node._value == value:
      return true
    case let .some(node):
      return contains(value < node._value ? node._left : node._right,
        value)
    }
  }
}

extension Tree: Sequence {
  public typealias Iterator = AnyIterator<T>

  public func makeIterator() -> Iterator {
    var stack: [Node] = []
    var current: Node? = _root
    return AnyIterator {
      // stack-based technique for inorder traversal
      // without recursion
      while true {
        if let node = current {
          stack.append(node)
          current = node._left
        }
        else if !stack.isEmpty {
          let pop = stack.removeLast()
          current = pop._right
          return pop._value
        }
        else {
          return nil
        }
      }
    }
  }
}
