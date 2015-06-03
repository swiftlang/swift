//===--- Map.swift - tests for lazy mapping -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// CHECK: testing...
print("testing...")

// Test mapping a collection
// CHECK-NEXT: [6, 9, 12, 15, 18, 21]
let a = lazy(2..<8).map { $0 * 3 }.array
print(a)

// Test mapping a sequence
let s = lazy(a.generate()).map { $0 / 3 }
// CHECK-NEXT: <2, 3, 4, 5, 6, 7>
print("<", appendNewline: false)
var prefix = ""
for x in s {
  print("\(prefix)\(x)", appendNewline: false)
  prefix = ", "
}
print(">")

//===--- Avoid creating gratutitously self-destructive sequences ----------===//

// In a naive implementation, mapping over a non-self-destructive
// SequenceType having a reference-semantics GeneratorType produces a
// self-destructive mapped view.  This is technically correct because
// Sequences are allowed to be self-destructive, and theoretically
// every multi-pass SequenceType would be a CollectionType, but Sequences are
// much easier to build than Collections and it would be extremely
// surprising for users if their mappings were not stable.

// A GeneratorType with reference semantics
class Counter : GeneratorType {
  func next() -> Int? {
    return n < end ? n++ : nil
  }

  init(_ n: Int, _ end: Int) {
    self.n = n
    self.end = end
  }
  
  var n: Int
  var end: Int
}

// A SequenceType with value semantics
struct IntRange : SequenceType {
  func generate() -> Counter {
    return Counter(start, end)
  }
  
  var start: Int
  var end: Int
}


// Make sure we can iterate a mapped view of IntRange without
// consuming it.
let m1 = lazy(IntRange(start: 1, end: 5)).map { $0 * 2 }
// CHECK-NEXT: [2, 4, 6, 8]
print(Array(m1))

// A second iteration produces the same result.
// CHECK-NEXT: [2, 4, 6, 8]
print(Array(m1))

// CHECK-NEXT: all done.
print("all done.")
