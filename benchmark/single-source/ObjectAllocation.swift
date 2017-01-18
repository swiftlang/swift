//===--- ObjectAllocation.swift -------------------------------------------===//
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

// This test checks the performance of allocations.
import TestsUtils

final class XX {
  var xx: Int

  init(_ x: Int) {
    xx = x
  }
}

final class TreeNode {
  let left: XX
  let right: XX

  init(_ l: XX, _ r: XX) {
    left = l
    right = r
  }
}

final class LinkedNode {
  var next: LinkedNode?
  var xx: Int

  init(_ x: Int, _ n: LinkedNode?) {
    xx = x
    next = n
  }
}

@inline(never)
func getInt(_ x: XX) -> Int {
  return x.xx
}

@inline(never)
func testSingleObject() -> Int {
  var s = 0
  for i in 0..<1000 {
    let x = XX(i)
    s += getInt(x)
  }
  return s
}

@inline(never)
func addInts(_ t: TreeNode) -> Int {
  return t.left.xx + t.right.xx
}

@inline(never)
func testTree() -> Int {
  var s = 0
  for i in 0..<300 {
    let t = TreeNode(XX(i), XX(i + 1))
    s += addInts(t)
  }
  return s
}

@inline(never)
func addAllInts(_ n: LinkedNode) -> Int {
  var s = 0
  var iter: LinkedNode? = n
  while let iter2 = iter {
     s += iter2.xx
     iter = iter2.next
  }
  return s
}

@inline(never)
func testList() -> Int {
  var s = 0
  for i in 0..<250 {
    let l = LinkedNode(i, LinkedNode(27, LinkedNode(42, nil)))
    s += addAllInts(l)
  }
  return s
}

@inline(never)
func identity(_ x: Int) -> Int {
  return x
}

@inline(never)
func testArray() -> Int {
  var s = 0
  for _ in 0..<1000 {
    for i in [0, 1, 2] {
      s += identity(i)
    }
  }
  return s
}

@inline(never)
public func run_ObjectAllocation(_ N: Int) {

  var SingleObjectResult = 0
  var TreeResult = 0
  var ListResult = 0
  var ArrayResult = 0

  for _ in 0..<N {
    SingleObjectResult = testSingleObject()
    TreeResult = testTree()
    ListResult = testList()
    ArrayResult = testArray()
  }

  CheckResults(SingleObjectResult == 499500,
               "Incorrect results in testSingleObject")
  CheckResults(TreeResult == 90000,
               "Incorrect results in testTree")
  CheckResults(ListResult == 48375,
               "Incorrect results in testList")
  CheckResults(ArrayResult == 3000,
               "Incorrect results in testArray")
}

