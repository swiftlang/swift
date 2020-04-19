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

import TestsUtils

// This test checks the performance of allocations.
// 53% _swift_release_dealloc
// 30% _swift_alloc_object
// 10% retain/release
public var ObjectAllocation = BenchmarkInfo(
  name: "ObjectAllocation",
  runFunction: run_ObjectAllocation,
  tags: [.runtime, .cpubench]
)

@inline(never)
func dummy<T>(_ x: T) -> T{
  return x
}

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

@inline(never)
func addInts(_ t: TreeNode) -> Int {
  return t.left.xx + t.right.xx
}

func testTree() -> Int {
  var s = 0
  for i in 0..<300 {
    let t = TreeNode(XX(i), XX(i + 1))
    s += addInts(t)
  }
  return s
}

@inline(never)
public func run_ObjectAllocation(_ N: Int) {

  var TreeResult = 0
  

  for _ in 0..<N {

    TreeResult = testTree()
    
  }
  
  _ = dummy(TreeResult == 90000)
}
