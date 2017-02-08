//===--- LinkedList.swift -------------------------------------------------===//
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

// This test checks performance of linked lists. It is based on LinkedList from
// utils/benchmark, with modifications for performance measuring.
import TestsUtils

final class Node {
  var next: Node?
  var data: Int

  init(n: Node?, d: Int) {
    next = n
    data = d
  }
}

@inline(never)
public func run_LinkedList(_ N: Int) {
  let size = 100
  var head = Node(n:nil, d:0)
  for i in 0..<size {
    head = Node(n:head, d:i)
  }

  var sum = 0
  let ref_result = size*(size-1)/2
  var ptr = head
  for _ in 1...5000*N {
    ptr = head
    sum = 0
    while let nxt = ptr.next {
      sum += ptr.data
      ptr = nxt
    }
    if sum != ref_result {
      break
    }
  }
  CheckResults(sum == ref_result,
               "Incorrect results in LinkedList: \(sum) != \(ref_result)")
}
