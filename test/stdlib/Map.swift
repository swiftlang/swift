//===--- Map.swift - tests for lazy mapping -------------------------------===//
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
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Check that the generic parameters are called 'Base' and 'Element'.
protocol TestProtocol1 {}

extension LazyMapIterator where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyMapSequence where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyMapCollection where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

// CHECK: testing...
print("testing...")

// Test mapping a collection
// CHECK-NEXT: [6, 9, 12, 15, 18, 21]
let a = Array((2..<8).lazy.map { $0 * 3 })
print(a)

// Test mapping a sequence
let s = a.makeIterator().lazy.map { $0 / 3 }
// CHECK-NEXT: <2, 3, 4, 5, 6, 7>
print("<", terminator: "")
var prefix = ""
for x in s {
  print("\(prefix)\(x)", terminator: "")
  prefix = ", "
}
print(">")

//===--- Avoid creating gratuitously self-destructive sequences -----------===//

// In a naive implementation, mapping over a non-self-destructive
// Sequence having a reference-semantics IteratorProtocol produces a
// self-destructive mapped view.  This is technically correct because
// Sequences are allowed to be self-destructive, and theoretically
// every multi-pass Sequence would be a Collection, but Sequences are
// much easier to build than Collections and it would be extremely
// surprising for users if their mappings were not stable.

// An IteratorProtocol with reference semantics
class Counter : IteratorProtocol {
  func next() -> Int? {
    if n >= end { return nil }
    n += 1
    return n-1
  }

  init(_ n: Int, _ end: Int) {
    self.n = n
    self.end = end
  }
  
  var n: Int
  var end: Int
}

// A Sequence with value semantics
struct IntRange : Sequence {
  func makeIterator() -> Counter {
    return Counter(start, end)
  }
  
  var start: Int
  var end: Int
}


// Make sure we can iterate a mapped view of IntRange without
// consuming it.
let m1 = IntRange(start: 1, end: 5).lazy.map { $0 * 2 }
// CHECK-NEXT: [2, 4, 6, 8]
print(Array(m1))

// A second iteration produces the same result.
// CHECK-NEXT: [2, 4, 6, 8]
print(Array(m1))

// lazy.map.map chain should fold two layers of LazyMapSequence
func foldingLevelsSequence<S : Sequence>(_ xs: S) {
  let result = xs.lazy.map { $0 }.map { $0 }
  print(type(of: result))
}
// CHECK-NEXT: LazyMapSequence<Array<Int>, Int>
foldingLevelsSequence(Array(0..<10))

// ... but the old way should also be available given explicit type context
func backwardCompatibleSequence<S : Sequence>(_ xs: S) {
  typealias ExpectedType = LazyMapSequence<LazyMapSequence<S, S.Element>, S.Element>
  let result: ExpectedType = xs.lazy.map { $0 }.map { $0 }
  print(type(of: result))
}
// CHECK-NEXT: LazyMapSequence<LazyMapSequence<Array<Int>, Int>, Int>
backwardCompatibleSequence(Array(0..<10))

// lazy.map.map chain should fold two layers of LazyMapCollection
func foldingLevelsCollection<C : Collection>(_ xs: C) {
  let result = xs.lazy.map { $0 }.map { $0 }
  print(type(of: result))
}
// CHECK-NEXT: LazyMapCollection<Array<Int>, Int>
foldingLevelsCollection(Array(0..<10))

// ... but the old way should also be available given explicit type context
func backwardCompatibleCollection<C : Collection>(_ xs: C) {
  typealias ExpectedType =
    LazyMapCollection<LazyMapCollection<C, C.Element>, C.Element>
  let result: ExpectedType = xs.lazy.map { $0 }.map { $0 }
  print(type(of: result))
}
// CHECK-NEXT: LazyMapCollection<LazyMapCollection<Array<Int>, Int>, Int>
backwardCompatibleCollection(Array(0..<10))

// CHECK-NEXT: all done.
print("all done.")
