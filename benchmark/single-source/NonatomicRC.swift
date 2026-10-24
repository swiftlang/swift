//===--- NonatomicRC.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Reference counting on classes marked @_nonatomicReferenceCounting, next to
// identical classes that are not. Each workload retains and releases its nodes
// heavily on a single thread.

import TestsUtils

public let benchmarks: [BenchmarkInfo] = {
  var result: [BenchmarkInfo] = []
  func add(_ name: String, _ fn: @escaping (Int) -> Void,
           _ setUp: (() -> Void)? = nil) {
    result.append(BenchmarkInfo(name: name, runFunction: fn,
                                tags: [.refcount, .runtime],
                                setUpFunction: setUp))
  }
  add("NonatomicRC.Atomic.ListTraversal", run_AtomicListTraversal, { blackHole(aList) })
  add("NonatomicRC.Nonatomic.ListTraversal", run_NonatomicListTraversal, { blackHole(nList) })
  add("NonatomicRC.Atomic.TreeWalk", run_AtomicTreeWalk, { blackHole(aTree) })
  add("NonatomicRC.Nonatomic.TreeWalk", run_NonatomicTreeWalk, { blackHole(nTree) })
  add("NonatomicRC.Atomic.PassThrough", run_AtomicPassThrough)
  add("NonatomicRC.Nonatomic.PassThrough", run_NonatomicPassThrough)
  add("NonatomicRC.Atomic.AllocFree", run_AtomicAllocFree)
  add("NonatomicRC.Nonatomic.AllocFree", run_NonatomicAllocFree)
  add("NonatomicRC.Atomic.ArrayOfRef", run_AtomicArrayOfRef)
  add("NonatomicRC.Nonatomic.ArrayOfRef", run_NonatomicArrayOfRef)
  return result
}()

// MARK: Atomic

final class AtomicNode {
  var next: AtomicNode?
  var children: [AtomicNode] = []
  var value: Int
  init(value: Int, next: AtomicNode? = nil) {
    self.value = value
    self.next = next
  }
}

let aList: AtomicNode = {
  var head: AtomicNode? = nil
  for i in 0..<1000 { head = AtomicNode(value: i, next: head) }
  return head!
}()

let aTree: AtomicNode = {
  func make(_ depth: Int, _ value: inout Int) -> AtomicNode {
    let n = AtomicNode(value: value)
    value &+= 1
    if depth > 0 {
      for _ in 0..<4 { n.children.append(make(depth - 1, &value)) }
    }
    return n
  }
  var v = 0
  return make(5, &v)
}()

@inline(never)
public func run_AtomicListTraversal(_ n: Int) {
  var total = 0
  for _ in 0..<(n * 200) {
    var cur: AtomicNode? = identity(aList)
    while let node = cur {
      total &+= node.value
      cur = node.next
    }
  }
  blackHole(total)
}

@inline(never)
func aSum(_ node: AtomicNode) -> Int {
  var total = identity(node).value
  for child in node.children { total &+= aSum(child) }
  return total
}

@inline(never)
public func run_AtomicTreeWalk(_ n: Int) {
  var total = 0
  for _ in 0..<(n * 20) { total &+= aSum(identity(aTree)) }
  blackHole(total)
}

@inline(never)
func aPass(_ a: AtomicNode, _ b: AtomicNode) -> AtomicNode {
  return a.value > b.value ? a : b
}

@inline(never)
public func run_AtomicPassThrough(_ n: Int) {
  let a = AtomicNode(value: 1), b = AtomicNode(value: 2)
  var total = 0
  for _ in 0..<(n * 2000) {
    total &+= aPass(identity(a), identity(b)).value
  }
  blackHole(total)
}

@inline(never)
public func run_AtomicAllocFree(_ n: Int) {
  var total = 0
  for i in 0..<(n * 200) {
    var head: AtomicNode? = nil
    for j in 0..<16 { head = AtomicNode(value: i &+ j, next: head) }
    total &+= head!.value
  }
  blackHole(total)
}

@inline(never)
public func run_AtomicArrayOfRef(_ n: Int) {
  var array: [AtomicNode] = []
  for i in 0..<512 { array.append(AtomicNode(value: i)) }
  var total = 0
  for _ in 0..<(n * 40) {
    for node in array { total &+= identity(node).value }
  }
  blackHole(total)
}

// MARK: Nonatomic

@_nonatomicReferenceCounting
final class NonatomicNode {
  var next: NonatomicNode?
  var children: [NonatomicNode] = []
  var value: Int
  init(value: Int, next: NonatomicNode? = nil) {
    self.value = value
    self.next = next
  }
}

let nList: NonatomicNode = {
  var head: NonatomicNode? = nil
  for i in 0..<1000 { head = NonatomicNode(value: i, next: head) }
  return head!
}()

let nTree: NonatomicNode = {
  func make(_ depth: Int, _ value: inout Int) -> NonatomicNode {
    let n = NonatomicNode(value: value)
    value &+= 1
    if depth > 0 {
      for _ in 0..<4 { n.children.append(make(depth - 1, &value)) }
    }
    return n
  }
  var v = 0
  return make(5, &v)
}()

@inline(never)
public func run_NonatomicListTraversal(_ n: Int) {
  var total = 0
  for _ in 0..<(n * 200) {
    var cur: NonatomicNode? = identity(nList)
    while let node = cur {
      total &+= node.value
      cur = node.next
    }
  }
  blackHole(total)
}

@inline(never)
func nSum(_ node: NonatomicNode) -> Int {
  var total = identity(node).value
  for child in node.children { total &+= nSum(child) }
  return total
}

@inline(never)
public func run_NonatomicTreeWalk(_ n: Int) {
  var total = 0
  for _ in 0..<(n * 20) { total &+= nSum(identity(nTree)) }
  blackHole(total)
}

@inline(never)
func nPass(_ a: NonatomicNode, _ b: NonatomicNode) -> NonatomicNode {
  return a.value > b.value ? a : b
}

@inline(never)
public func run_NonatomicPassThrough(_ n: Int) {
  let a = NonatomicNode(value: 1), b = NonatomicNode(value: 2)
  var total = 0
  for _ in 0..<(n * 2000) {
    total &+= nPass(identity(a), identity(b)).value
  }
  blackHole(total)
}

@inline(never)
public func run_NonatomicAllocFree(_ n: Int) {
  var total = 0
  for i in 0..<(n * 200) {
    var head: NonatomicNode? = nil
    for j in 0..<16 { head = NonatomicNode(value: i &+ j, next: head) }
    total &+= head!.value
  }
  blackHole(total)
}

@inline(never)
public func run_NonatomicArrayOfRef(_ n: Int) {
  var array: [NonatomicNode] = []
  for i in 0..<512 { array.append(NonatomicNode(value: i)) }
  var total = 0
  for _ in 0..<(n * 40) {
    for node in array { total &+= identity(node).value }
  }
  blackHole(total)
}
