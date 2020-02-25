//===--- RandomTree.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test implements three competing versions of randomized binary trees,
// indirectly testing reference counting performance.

import TestsUtils

var rng = SplitMix64(seed: 0)
let count = 10_000
let input = (0 ..< count).shuffled(using: &rng)

public let RandomTree = [
  BenchmarkInfo(
    name: "RandomTree.insert.ADT",
    runFunction: run_ADT_insert,
    tags: [.validation, .algorithm, .refcount],
    setUpFunction: { blackHole(input) }),
  BenchmarkInfo(
    name: "RandomTree.insert.Unmanaged",
    runFunction: run_Unmanaged_insert,
    tags: [.validation, .algorithm, .refcount],
    setUpFunction: { blackHole(input) }),
  BenchmarkInfo(
    name: "RandomTree.insert.UnsafePointer",
    runFunction: run_UnsafePointer_insert,
    tags: [.validation, .algorithm, .refcount],
    setUpFunction: { blackHole(input) }),
]

enum EnumSearchTree<Element: Comparable> {
  case empty
  indirect case node(EnumSearchTree<Element>, Element, EnumSearchTree<Element>)
}

extension EnumSearchTree {
  func forEach(_ body: (Element) -> Void) {
    switch self {
    case .empty:
      break
    case let .node(left, value, right):
      left.forEach(body)
      body(value)
      right.forEach(body)
    }
  }

  func contains(_ value: Element) -> Bool {
    switch self {
    case .empty:
      return false
    case let .node(left, v, right):
      if value == v { return true }
      return value < v ? left.contains(value) : right.contains(value)
    }
  }

  func inserting(_ value: __owned Element) -> EnumSearchTree {
    switch self {
    case .empty:
      return .node(.empty, value, .empty)
    case let .node(left, root, right):
      if value == root {
        return self
      } else if value < root {
        return .node(left.inserting(value), root, right)
      } else {
        return .node(left, root, right.inserting(value))
      }
    }
  }
}

struct UnmanagedSearchTree<Element: Comparable> {
  class Node {
    var value: Element
    var left: UnmanagedSearchTree
    var right: UnmanagedSearchTree

    init(
      value: Element,
      left: UnmanagedSearchTree = .empty,
      right: UnmanagedSearchTree = .empty
    ) {
      self.left = left
      self.right = right
      self.value = value
    }
  }

  static var empty: UnmanagedSearchTree<Element> { UnmanagedSearchTree() }

  var root: Unmanaged<Node>?

  init() {
    self.root = nil
  }

  init(_root: Unmanaged<Node>?) {
    self.root = _root
  }
}

extension UnmanagedSearchTree {
  mutating func deallocate() {
    guard let root = root else { return }
    root._withUnsafeGuaranteedRef { root in
      root.left.deallocate()
      root.right.deallocate()
    }
    withExtendedLifetime(root) {}
  }
}

extension UnmanagedSearchTree {
  func forEach(_ body: (Element) -> Void) {
    guard let root = root else { return }
    root._withUnsafeGuaranteedRef { root in
      root.left.forEach(body)
      body(root.value)
      root.right.forEach(body)
    }
  }

  func contains(_ value: Element) -> Bool {
    guard let root = root else { return false }
    return root._withUnsafeGuaranteedRef { root in
      if value == root.value { return true }
      return value < root.value
        ? root.left.contains(value)
        : root.right.contains(value)
    }
  }

  mutating func insert(_ value: __owned Element) {
    guard let root = root else {
      self.root = Unmanaged.passRetained(Node(value: value))
      return
    }
    root._withUnsafeGuaranteedRef { root in
      if value == root.value {
        return
      } else if value < root.value {
        root.left.insert(value)
      } else {
        root.right.insert(value)
      }
    }
  }
}

struct PointerSearchTree<Element: Comparable> {
  struct Node {
    var value: Element
    var left: PointerSearchTree = .empty
    var right: PointerSearchTree = .empty
  }

  static var empty: PointerSearchTree<Element> { PointerSearchTree() }

  var root: UnsafeMutablePointer<Node>?

  init() {
    self.root = nil
  }

  init(_root: UnsafeMutablePointer<Node>?) {
    self.root = _root
  }
}

extension PointerSearchTree {
  mutating func deallocate() {
    guard let root = root else { return }
    root.pointee.left.deallocate()
    root.pointee.right.deallocate()
  }
}

extension PointerSearchTree {
  func forEach(_ body: (Element) -> Void) {
    guard let root = root else { return }
    root.pointee.left.forEach(body)
    body(root.pointee.value)
    root.pointee.right.forEach(body)
  }

  func contains(_ value: Element) -> Bool {
    guard let root = root else { return false }
    if value == root.pointee.value { return true }
    if value < root.pointee.value { return root.pointee.left.contains(value) }
    return root.pointee.right.contains(value)
  }

  mutating func insert(_ value: __owned Element) {
    guard let root = root else {
      let node = UnsafeMutablePointer<Node>.allocate(capacity: 1)
      node.initialize(to: Node(value: value))
      self.root = node
      return
    }
    if value == root.pointee.value {
      return
    } else if value < root.pointee.value {
      root.pointee.left.insert(value)
    } else {
      root.pointee.right.insert(value)
    }
  }
}



func run_ADT_insert(_ iterations: Int) {
  for _ in 0 ..< iterations {
    var tree = identity(EnumSearchTree<Int>.empty)
    for value in input {
      tree = tree.inserting(value)
    }
    blackHole(tree)
  }
}

func run_Unmanaged_insert(_ iterations: Int) {
  for _ in 0 ..< iterations {
    var tree = identity(UnmanagedSearchTree<Int>.empty)
    for value in input {
      tree.insert(value)
    }
    blackHole(tree)
    tree.deallocate()
  }
}

func run_UnsafePointer_insert(_ iterations: Int) {
  for _ in 0 ..< iterations {
    var tree = identity(PointerSearchTree<Int>.empty)
    for value in input {
      tree.insert(value)
    }
    blackHole(tree)
    tree.deallocate()
  }
}

