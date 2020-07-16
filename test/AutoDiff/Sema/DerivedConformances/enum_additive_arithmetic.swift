// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

import _Differentiation

func testAdditiveArithmetic<T: AdditiveArithmetic>(
  _ x: inout T, _ y: T
) {
  // Test `+`, `-`.
  x += x + y
  x -= x - y
}

// Test empty enum
enum Empty: AdditiveArithmetic {}
func testEmpty() {
  // Should typecheck successfully, only runtime error happens:
  let emptyZero: Empty = Empty.zero
  let _ = emptyZero + emptyZero
}

enum Ints: AdditiveArithmetic {
  case branch(Int, Int)
  case leaf(Int)
  case none
}
func testInts() {
  var ints1 = Ints.branch(1, 2)
  let ints2 = Ints.branch(2, 1)
  testAdditiveArithmetic(&ints1, ints2)
  var ints3 = Ints.leaf(2)
  let ints4 = Ints.leaf(1)
  testAdditiveArithmetic(&ints3, ints4)
  var ints5 = Ints.none
  let ints6 = Ints.none
  testAdditiveArithmetic(&ints5, ints6)
  // Should typecheck successfully, only runtime error happens:
  testAdditiveArithmetic(&ints5, ints1)
}

// Test indirect enum.
indirect enum Tree: AdditiveArithmetic {
  case branch(Int, Tree, Int)
  case leaf(Int)
}
func testTree() {
  let leaf1 = Tree.leaf(1)
  var tree1 = Tree.branch(1, leaf1, 1)
  let leaf2 = Tree.leaf(2)
  let tree2 = Tree.branch(2, leaf2, 2)
  testAdditiveArithmetic(&tree1, tree2)
}

// Test generic indirect enum.
indirect enum GenericTree<T: AdditiveArithmetic>: AdditiveArithmetic {
  case branch(T, GenericTree, T)
  case leaf(T)
}
func testGenericTree() {
  let leaf1 = GenericTree<Int>.leaf(1)
  var tree1 = GenericTree<Int>.branch(1, leaf1, 1)
  let leaf2 = GenericTree<Int>.leaf(2)
  let tree2 = GenericTree<Int>.branch(2, leaf2, 2)
  testAdditiveArithmetic(&tree1, tree2) 
}
