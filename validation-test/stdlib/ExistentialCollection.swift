//===--- ExistentialCollection.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

// Check that the generic parameter is called 'Element'.
protocol TestProtocol1 {}

extension AnyIterator where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension AnySequence where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension AnyCollection where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension AnyBidirectionalCollection where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension AnyRandomAccessCollection where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

var tests = TestSuite("ExistentialCollection")

tests.test("AnyIterator") {
  func digits() -> AnyIterator<String> {
    let lazyStrings = (0..<5).lazy.map { String($0) }
    
    // This is a really complicated type of no interest to our
    // clients.
    let iterator: LazyMapIterator<
        IndexingIterator<RangeOfStrideable<Int>>, String
      > = lazyStrings.makeIterator()
    return AnyIterator(iterator)
  }
  expectEqual(["0", "1", "2", "3", "4"], Array(digits()))

  var x = 7
  let iterator = AnyIterator<Int> {
    if x >= 15 { return nil }
    x += 1
    return x-1
  }
  expectEqual([ 7, 8, 9, 10, 11, 12, 13, 14 ], Array(iterator))
}

let initialCallCounts = [
  "successor": 0, "predecessor": 0,
  "_successorInPlace": 0, "_predecessorInPlace": 0,
  "advance(_:by:)": 0, "distance(from:to:)": 0
]

var callCounts = initialCallCounts

struct InstrumentedCollection<
  C : RandomAccessCollection
> : RandomAccessCollection {

  typealias Index = C.Index
  typealias Iterator = C.Iterator
  typealias IndexDistance = C.IndexDistance

  typealias SubSequence = C.SubSequence
  typealias Indices = C.Indices

  var base: C

  init(_ base: C) {
    self.base = base
  }

  static func resetCounts() {
    callCounts = initialCallCounts
  }

  @warn_unused_result
  func next(i: Index) -> Index {
    callCounts["successor"]! += 1
    return base.next(i)
  }

  func _nextInPlace(i: inout Index) {
    callCounts["_successorInPlace"]! += 1
    base._nextInPlace(&i)
  }

  @warn_unused_result
  func previous(i: Index) -> Index {
    callCounts["predecessor"]! += 1
    return base.previous(i)
  }

  func _previousInPlace(i: inout Index) {
    callCounts["_predecessorInPlace"]! += 1
    base._previousInPlace(&i)
  }

  func index(n: IndexDistance, stepsFrom i: Index) -> Index {
    callCounts["advance(_:by:)"]! += 1
    return base.index(n, stepsFrom: i)
  }

  func distance(from start: Index, to end: Index) -> IndexDistance {
    callCounts["distance(from:to:)"]! += 1
    return base.distance(from: start, to: end)
  }

  var startIndex: Index {
    return base.startIndex
  }

  var endIndex: Index {
    return base.endIndex
  }

  subscript(i: Index) -> Iterator.Element {
    fatalError()
  }

  subscript(bounds: Range<Index>) -> SubSequence {
    fatalError()
  }

  func makeIterator() -> Iterator {
    return base.makeIterator()
  }

  var indices: Indices {
    return base.indices
  }

}

tests.test("AnySequence.init(Sequence)") {
  if true {
    let base = MinimalSequence<OpaqueValue<Int>>(elements: [])
    var s = AnySequence(base)
    expectType(AnySequence<OpaqueValue<Int>>.self, &s)
    checkSequence([], s, resiliencyChecks: .none) { $0.value == $1.value }
  }
  if true {
    let intData = [ 1, 2, 3, 5, 8, 13, 21 ]
    let data = intData.map(OpaqueValue.init)
    let base = MinimalSequence(elements: data)
    var s = AnySequence(base)
    expectType(AnySequence<OpaqueValue<Int>>.self, &s)
    checkSequence(data, s, resiliencyChecks: .none) { $0.value == $1.value }
  }
}

tests.test("AnySequence.init(() -> Generator)") {
  if true {
    var s = AnySequence {
      return MinimalIterator<OpaqueValue<Int>>([])
    }
    expectType(AnySequence<OpaqueValue<Int>>.self, &s)
    checkSequence([], s, resiliencyChecks: .none) { $0.value == $1.value }
  }
  if true {
    let intData = [ 1, 2, 3, 5, 8, 13, 21 ]
    let data = intData.map(OpaqueValue.init)
    var s = AnySequence {
      return MinimalIterator(data)
    }
    expectType(AnySequence<OpaqueValue<Int>>.self, &s)
    checkSequence(data, s, resiliencyChecks: .none) { $0.value == $1.value }
  }
}

tests.test("AnyCollection successor/predecessor") {
  let c = AnyCollection(InstrumentedCollection(0..<10))
  var i = c.startIndex

  i = c.next(i)
  expectEqual(1, callCounts["successor"])
  expectEqual(0, callCounts["_successorInPlace"])

  c._nextInPlace(&i)
  expectEqual(1, callCounts["successor"])
  expectEqual(1, callCounts["_successorInPlace"])

  var x = i
  i = c.next(i)
  expectEqual(2, callCounts["successor"])
  expectEqual(1, callCounts["_successorInPlace"])
  _blackHole(x)
}

tests.test("AnyBidirectionalCollection successor/predecessor") {
  let c = AnyBidirectionalCollection(InstrumentedCollection(0..<10))
  var i = c.startIndex

  expectEqual(0, callCounts["predecessor"])
  expectEqual(0, callCounts["_predecessorInPlace"])

  i = c.previous(i)
  expectEqual(1, callCounts["predecessor"])
  expectEqual(0, callCounts["_predecessorInPlace"])

  c._previousInPlace(&i)
  expectEqual(1, callCounts["predecessor"])
  expectEqual(1, callCounts["_predecessorInPlace"])

  var x = i
  i = c.previous(i)
  expectEqual(2, callCounts["predecessor"])
  expectEqual(1, callCounts["_predecessorInPlace"])
  _blackHole(x)
}

tests.test("ForwardCollection") {
  let a0: ContiguousArray = [1, 2, 3, 5, 8, 13, 21]
  let fc0 = AnyCollection(a0)
  let a1 = ContiguousArray(fc0)
  expectEqual(a0, a1)
  for e in a0 {
    let i = fc0.index(of: e)
    expectNotEmpty(i)
    expectEqual(e, fc0[i!])
  }
  for i in fc0.indices {
    expectNotEqual(fc0.endIndex, i)
    expectEqual(1, fc0.indices.filter { $0 == i }.count)
  }
}

tests.test("BidirectionalCollection") {
  let a0: ContiguousArray = [1, 2, 3, 5, 8, 13, 21]
  let fc0 = AnyCollection(a0.lazy.reversed())
  
  let bc0_ = AnyBidirectionalCollection(fc0)  // upgrade!
  expectNotEmpty(bc0_)
  let bc0 = bc0_!
  expectTrue(fc0 === bc0)

  let fc1 = AnyCollection(a0.lazy.reversed()) // new collection
  expectFalse(fc1 === fc0)

  let fc2 = AnyCollection(bc0)                // downgrade
  expectTrue(fc2 === bc0)
  
  let a1 = ContiguousArray(bc0.lazy.reversed())
  expectEqual(a0, a1)
  for e in a0 {
    let i = bc0.index(of: e)
    expectNotEmpty(i)
    expectEqual(e, bc0[i!])
  }
  for i in bc0.indices {
    expectNotEqual(bc0.endIndex, i)
    expectEqual(1, bc0.indices.filter { $0 == i }.count)
  }
  
  // Can't upgrade a non-random-access collection to random access
  let s0 = "Hello, Woyld".characters
  let bc1 = AnyBidirectionalCollection(s0)
  let fc3 = AnyCollection(bc1)
  expectTrue(fc3 === bc1)
  expectEmpty(AnyRandomAccessCollection(bc1))
  expectEmpty(AnyRandomAccessCollection(fc3))
}

tests.test("RandomAccessCollection") {
  let a0: ContiguousArray = [1, 2, 3, 5, 8, 13, 21]
  let fc0 = AnyCollection(a0.lazy.reversed())
  let rc0_ = AnyRandomAccessCollection(fc0)         // upgrade!
  expectNotEmpty(rc0_)
  let rc0 = rc0_!
  expectTrue(rc0 === fc0)

  let bc1 = AnyBidirectionalCollection(rc0)         // downgrade
  expectTrue(bc1 === rc0)

  let fc1 = AnyBidirectionalCollection(rc0)         // downgrade
  expectTrue(fc1 === rc0)
  
  let a1 = ContiguousArray(rc0.lazy.reversed())
  expectEqual(a0, a1)
  for e in a0 {
    let i = rc0.index(of: e)
    expectNotEmpty(i)
    expectEqual(e, rc0[i!])
  }
  for i in rc0.indices {
    expectNotEqual(rc0.endIndex, i)
    expectEqual(1, rc0.indices.filter { $0 == i }.count)
  }
}

runAllTests()
