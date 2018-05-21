//===--- RangeAssignment.swift --------------------------------------------===//
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

public let QueueGeneric = BenchmarkInfo(
  name: "QueueGeneric",
  runFunction: run_QueueGeneric,
  tags: [.validation, .api],
  setUpFunction: { buildWorkload() },
  tearDownFunction: nil)

public let QueueConcrete = BenchmarkInfo(
  name: "QueueConcrete",
  runFunction: run_QueueConcrete,
  tags: [.validation, .api],
  setUpFunction: { buildWorkload() },
  tearDownFunction: nil)
 
// TODO: remove when there is a native equivalent in the std lib
extension RangeReplaceableCollection where Self: BidirectionalCollection {
  public mutating func popLast() -> Element? {
    if isEmpty { return nil}
    else { return removeLast() }
  }
}

public struct Queue<Storage: RangeReplaceableCollection>
where Storage: BidirectionalCollection {
  public typealias Element = Storage.Element

  internal var _in: Storage
  internal var _out: Storage
  
  public init() {
    _in = Storage()
    _out = Storage()
  }
}

extension Queue {  
  public mutating func enqueue(_ newElement: Element) {
    _in.append(newElement)
  }

  public mutating func dequeue() -> Element? {
    if _out.isEmpty {
      _out.append(contentsOf: _in.reversed())
      _in.removeAll()
    }
    return _out.popLast()
  }
}

func testQueue<Elements: Collection>(elements: Elements)
where Elements.Element: Equatable {
  var q = Queue<[Elements.Element]>()
  for x in elements { q.enqueue(x) }
  let results = sequence(state: q) { $0.dequeue() }
  let i = results.reduce(0, { i,_ in i &+ 1 })
  for x in elements { q.enqueue(x) }
  let j = results.reduce(i, { i,_ in i &+ 1 })
  CheckResults(j == elements.count*2)
}

let n = 10_000
let workload = (0..<n).map { "\($0): A long enough string to defeat the SSO, or so I hope." }

public func buildWorkload() {
  let contents = workload
  _ = contents.reduce(0) { $0 + $1.count }
}

@inline(never)
func run_QueueGeneric(_ scale: Int) {
  for _ in 0..<scale {
    testQueue(elements: workload)
  }
}

public struct ConcreteQueue {
  internal var _in: [String]
  internal var _out: [String]
  
  public init() {
    _in = Array()
    _out = Array()
  }
}

extension ConcreteQueue {  
  public mutating func enqueue(_ newElement: String) {
    _in.append(newElement)
  }

  public mutating func dequeue() -> String? {
    if _out.isEmpty {
      _out.append(contentsOf: _in.reversed())
      _in.removeAll()
    }
    return _out.popLast()
  }
}

func testConcreteQueue(elements: [String]) {
  var q = ConcreteQueue()
  for x in elements { q.enqueue(x) }
  let results = sequence(state: q) { $0.dequeue() }
  let i = results.reduce(0, { i,_ in i &+ 1 })
  for x in elements { q.enqueue(x) }
  let j = results.reduce(i, { i,_ in i &+ 1 })
  CheckResults(j == elements.count*2)
}


@inline(never)
func run_QueueConcrete(_ scale: Int) {
  for _ in 0..<scale {
    testConcreteQueue(elements: workload)
  }
}

