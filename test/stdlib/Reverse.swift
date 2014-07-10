//===--- Reverse.swift - Tests for lazy and eager reverse() ---------------===//
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

// Reverse a random access Collection 
let r = lazy(2..<8).map { $0 * 3 }.reverse()

// CHECK-NEXT: [6, 9, 12, 15, 18, 21]
println(r.array)

func assertRandomAccess<T: RandomAccessIndex>(x: T) {}
assertRandomAccess(r.startIndex)

// The global reverse() function produces an Array.
let eager: Array = reverse(lazy(2..<8).map { $0 * 3 })

// Make sure it has the right contents
// CHECK-NEXT: true
println(equal(eager, r))

let raboof = reduce(lazy("foobar").reverse(), "") {
  // FIXME: "$0 + $1" should work just fine here
  (s: String, c: Character) in s + c
}
// CHECK-NEXT: "raboof"
debugPrintln(raboof)

// Prove that the result is at least bidirectional, i.e. reversible
let foobar = reduce(lazy("foobar").reverse().reverse(), "") {
  // FIXME: "$0 + $1" should work just fine here
  (s: String, c: Character) in s + c
}
// CHECK-NEXT: "foobar"
debugPrintln(foobar)

// CHECK-NEXT: all done.
println("all done.")
