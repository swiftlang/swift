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

// FIXME(prext): remove this file when protocol extensions land.

// CHECK: testing...
println("testing...")

// Reverse a random access CollectionType 
let r = lazy(2..<8).map { $0 * 3 }.reverse()

// CHECK-NEXT: [21, 18, 15, 12, 9, 6]
println(r.array)

func assertRandomAccess<T : RandomAccessIndexType>(x: T) {}
assertRandomAccess(r.startIndex)

// The global reverse() function produces an Array.
let eager: Array = reverse(lazy(2..<8).map { $0 * 3 })

// Make sure it has the right contents
// CHECK-NEXT: true
println(equal(eager, r))

let raboof = reduce(lazy("foobar").reverse(), "") {
  (s: String, c: Character) in s + String(c)
}
// CHECK-NEXT: "raboof"
debugPrintln(raboof)

// Prove that the result is at least bidirectional, i.e. reversible
let foobar = reduce(lazy("foobar").reverse().reverse(), "") {
  (s: String, c: Character) in s + String(c)
}
// CHECK-NEXT: "foobar"
debugPrintln(foobar)

// CHECK-NEXT: all done.
println("all done.")
