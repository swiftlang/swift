//===--- Index.swift - tests for Index types and operations ---------------===//
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

struct F : ForwardIndex {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func succ() -> F {
    println("F.succ: \(x)")
    return F(x + 1)
  }
}

func ==(a: F, b: F) -> Bool {
  return a.x == b.x
}

struct B : BidirectionalIndex {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func succ() -> B {
    println("B.succ: \(x)")
    return B(x + 1)
  }
  func pred() -> B {
    println("B.pred: \(x)")
    return B(x - 1)
  }
}

func ==(a: B, b: B) -> Bool {
  return a.x == b.x
}

struct R : RandomAccessIndex {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func succ() -> R {
    println("R.succ: \(x)")
    return R(x + 1)
  }
  func pred() -> R {
    println("R.pred: \(x)")
    return R(x - 1)
  }
  func distanceTo(rhs: R) -> Int {
    println("R.distanceTo: \(x), \(rhs.x)")
    return rhs.x - x
  }
  func advancedBy(n: Int) -> R {
    println("R.advancedBy: \(x), \(n)")
    return R(x + n)
  }
}

func ==(a: R, b: R) -> Bool {
  return a.x == b.x
}

println("testing...")
// CHECK: testing...

println("<F>")
// CHECK-NEXT: <F>
println("[\(distance(F(10), F(10)))]")
// CHECK-NEXT: [0]
println("[\(distance(F(10), F(13)))]")
// CHECK-NEXT: F.succ: 10
// CHECK-NEXT: F.succ: 11
// CHECK-NEXT: F.succ: 12
// CHECK-NEXT: [3]
println("[\(advance(F(7), 2).x)]")
// CHECK-NEXT: F.succ: 7
// CHECK-NEXT: F.succ: 8
// CHECK-NEXT: [9]
println("[\(advance(F(3), 99, F(5)).x)]")
// CHECK-NEXT: F.succ: 3
// CHECK-NEXT: F.succ: 4
// CHECK-NEXT: [5]

println("<B>")
// CHECK-NEXT: <B>
println("[\(distance(B(10), B(10)))]")
// CHECK-NEXT: [0]
println("[\(distance(B(10), B(12)))]")
// CHECK-NEXT: B.succ: 10
// CHECK-NEXT: B.succ: 11
// CHECK-NEXT: [2]
println("[\(advance(B(7), 2).x)]")
// CHECK-NEXT: B.succ: 7
// CHECK-NEXT: B.succ: 8
// CHECK-NEXT: [9]
println("[\(advance(B(7), -3).x)]")
// CHECK-NEXT: B.pred: 7
// CHECK-NEXT: B.pred: 6
// CHECK-NEXT: B.pred: 5
// CHECK-NEXT: [4]
println("[\(advance(B(13), 99, B(17)).x)]")
// CHECK-NEXT: B.succ: 13
// CHECK-NEXT: B.succ: 14
// CHECK-NEXT: B.succ: 15
// CHECK-NEXT: B.succ: 16
// CHECK-NEXT: [17]
println("[\(advance(B(13), -99, B(11)).x)]")
// CHECK-NEXT: B.pred: 13
// CHECK-NEXT: B.pred: 12
// CHECK-NEXT: [11]


println("<R>")
// CHECK-NEXT: <R>
println("[\(distance(R(10), R(10)))]")
// CHECK-NEXT: R.distanceTo: 10, 10
// CHECK-NEXT: [0]
println("[\(distance(R(10), R(20)))]")
// CHECK-NEXT: R.distanceTo: 10, 20
// CHECK-NEXT: [10]
println("[\(advance(R(7), 2).x)]")
// CHECK-NEXT: R.advancedBy: 7, 2
// CHECK-NEXT: [9]
println("[\(advance(R(13), 99, R(17)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 17
// CHECK-NEXT: [17]
println("[\(advance(R(13), 3, R(17)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 17
// CHECK-NEXT: R.advancedBy: 13, 3
// CHECK-NEXT: [16]
println("[\(advance(R(13), -99, R(10)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 10
// CHECK-NEXT: [10]
println("[\(advance(R(13), -3, R(0)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 0
// CHECK-NEXT: R.advancedBy: 13, -3
// CHECK-NEXT: [10]

println("all done.")
// CHECK-NEXT: all done.

