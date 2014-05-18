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

// CHECK: testing...
println("testing...")

// Test mapping a collection
// CHECK-NEXT: [6, 9, 12, 15, 18, 21]
let a = Array(map(2..8) { $0 * 3 })
println(a)

// Test mapping a sequence
let s = map(a.generate()) { $0 / 3 }
// CHECK-NEXT: <2, 3, 4, 5, 6, 7>
print("<")
var prefix = ""
for x in s {
  print("\(prefix)\(x)")
  prefix = ", "
}
println(">")

//===--- Avoid creating gratutitously self-destructive sequences ----------===//

// In a naive implementation, mapping over a non-self-destructive
// Sequence having a reference-semantics Generator produces a
// self-destructive mapped view.  This is technically correct because
// Sequences are allowed to be self-destructive, and theoretically
// every multi-pass Sequence would be a Collection, but Sequences are
// much easier to build than Collections and it would be extremely
// surprising for users if their mappings were not stable.

// A Generator with reference semantics
class Counter : Generator {
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

// A Sequence with value semantics
struct IntRange: Sequence {
  func generate() -> Counter {
    return Counter(start, end)
  }
  
  var start: Int
  var end: Int
}


// Make sure we can iterate a mapped view of IntRange without
// consuming it.
let m1 = map(IntRange(start: 1, end: 5)) { $0 * 2 }
// CHECK-NEXT: [2, 4, 6, 8]
println(Array(m1))

// A second iteration produces the same result.
// CHECK-NEXT: [2, 4, 6, 8]
println(Array(m1))

// CHECK-NEXT: all done.
println("all done.")
