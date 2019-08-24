//===--- Reduce.swift - tests for the two reduce variants -----------------===//
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

// CHECK: testing...
print("testing...")

// Test the examples from the documentation of reduce(_:_:) and reduce(into:_:)

let numbers = [1, 2, 3, 4]
let numberSum = numbers.reduce(0, { x, y in
  x + y
})
// CHECK-NEXT: 10
print(numberSum)

let letters = "abracadabra"
let letterCount = letters.reduce(into: [:]) { counts, letter in
  counts[letter, default: 0] += 1
}
// CHECK-NEXT: ["a", "b", "c", "d", "r"]
print(letterCount.keys.sorted())
print(letterCount["a"]!) // CHECK: 5
print(letterCount["b"]!) // CHECK: 2
print(letterCount["c"]!) // CHECK: 1
print(letterCount["d"]!) // CHECK: 1
print(letterCount["r"]!) // CHECK: 2


// Test the two reduce methods with different levels of inference
let numbers2 = Array(2..<7)

// Test reduce(_:_:)
// CHECK-NEXT: 20
let sum1 = numbers2.reduce(0) { (x: Int, y: Int) -> Int in x + y }
print(sum1)

// CHECK-NEXT: 20
let sum2 = numbers2.reduce(0) { (x, y) in x + y }
print(sum2)

// CHECK-NEXT: 20
let sum3 = numbers2.reduce(0) { $0 + $1 }
print(sum3)

// CHECK-NEXT: 20
let sum4 = numbers2.reduce(0, +)
print(sum4)

// Test reduce(into:_:)
// CHECK-NEXT: 20
let sum5 = numbers2.reduce(into: 0) { (x: inout Int, y: Int) in x += y }
print(sum5)

// CHECK-NEXT: 20
let sum6 = numbers2.reduce(into: 0) { x, y in x += y }
print(sum6)

// CHECK-NEXT: 20
let sum7 = numbers2.reduce(into: 0) { $0 += $1 }
print(sum7)

// CHECK-NEXT: 20
let sum8 = numbers2.reduce(into: 0, +=)
print(sum8)


// Test that the initial value remains unmodified
var original = [0, 1]
let result = numbers2.reduce(into: original) { acc, x in
  acc.append(x)
}
// CHECK-NEXT: [0, 1]
print(original)
// CHECK-NEXT: [0, 1, 2, 3, 4, 5, 6]
print(result)


// CHECK: all done.
print("all done.")
