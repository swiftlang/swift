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

// CHECK-NEXT: all done.
println("all done.")
