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

struct F : ForwardIndexType {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func successor() -> F {
    print("F.successor: \(x)")
    return F(x + 1)
  }
}

func ==(a: F, b: F) -> Bool {
  return a.x == b.x
}

struct B : BidirectionalIndexType {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func successor() -> B {
    print("B.successor: \(x)")
    return B(x + 1)
  }
  func predecessor() -> B {
    print("B.predecessor: \(x)")
    return B(x - 1)
  }
}

func ==(a: B, b: B) -> Bool {
  return a.x == b.x
}

struct R : RandomAccessIndexType {
  var x: Int

  init(_ x: Int) {
    self.x = x
  }

  func successor() -> R {
    print("R.successor: \(x)")
    return R(x + 1)
  }
  func predecessor() -> R {
    print("R.predecessor: \(x)")
    return R(x - 1)
  }
  func distanceTo(rhs: R) -> Int {
    print("R.distanceTo: \(x), \(rhs.x)")
    return rhs.x - x
  }
  func advancedBy(n: Int) -> R {
    print("R.advancedBy: \(x), \(n)")
    return R(x + n)
  }
}

func ==(a: R, b: R) -> Bool {
  return a.x == b.x
}

print("testing...")
// CHECK: testing...

print("<F>")
// CHECK-NEXT: <F>
print("[\(distance(F(10), F(10)))]")
// CHECK-NEXT: [0]
print("[\(distance(F(10), F(13)))]")
// CHECK-NEXT: F.successor: 10
// CHECK-NEXT: F.successor: 11
// CHECK-NEXT: F.successor: 12
// CHECK-NEXT: [3]
print("[\(advance(F(7), 0).x)]")
// CHECK-NEXT: [7]
print("[\(advance(F(7), 2).x)]")
// CHECK-NEXT: F.successor: 7
// CHECK-NEXT: F.successor: 8
// CHECK-NEXT: [9]
print("[\(advance(F(3), 99, F(5)).x)]")
// CHECK-NEXT: F.successor: 3
// CHECK-NEXT: F.successor: 4
// CHECK-NEXT: [5]

print("<B>")
// CHECK-NEXT: <B>
print("[\(distance(B(10), B(10)))]")
// CHECK-NEXT: [0]
print("[\(distance(B(10), B(12)))]")
// CHECK-NEXT: B.successor: 10
// CHECK-NEXT: B.successor: 11
// CHECK-NEXT: [2]
print("[\(advance(B(7), 2).x)]")
// CHECK-NEXT: B.successor: 7
// CHECK-NEXT: B.successor: 8
// CHECK-NEXT: [9]
print("[\(advance(B(7), -3).x)]")
// CHECK-NEXT: B.predecessor: 7
// CHECK-NEXT: B.predecessor: 6
// CHECK-NEXT: B.predecessor: 5
// CHECK-NEXT: [4]
print("[\(advance(B(13), 0, B(17)).x)]")
// CHECK-NEXT: [13]
print("[\(advance(B(13), 99, B(17)).x)]")
// CHECK-NEXT: B.successor: 13
// CHECK-NEXT: B.successor: 14
// CHECK-NEXT: B.successor: 15
// CHECK-NEXT: B.successor: 16
// CHECK-NEXT: [17]
print("[\(advance(B(13), -99, B(11)).x)]")
// CHECK-NEXT: B.predecessor: 13
// CHECK-NEXT: B.predecessor: 12
// CHECK-NEXT: [11]


print("<R>")
// CHECK-NEXT: <R>
print("[\(distance(R(10), R(10)))]")
// CHECK-NEXT: R.distanceTo: 10, 10
// CHECK-NEXT: [0]
print("[\(distance(R(10), R(20)))]")
// CHECK-NEXT: R.distanceTo: 10, 20
// CHECK-NEXT: [10]
print("[\(advance(R(7), 2).x)]")
// CHECK-NEXT: R.advancedBy: 7, 2
// CHECK-NEXT: [9]
print("[\(advance(R(13), 99, R(17)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 17
// CHECK-NEXT: [17]
print("[\(advance(R(13), 3, R(17)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 17
// CHECK-NEXT: R.advancedBy: 13, 3
// CHECK-NEXT: [16]
print("[\(advance(R(13), -99, R(10)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 10
// CHECK-NEXT: [10]
print("[\(advance(R(13), -3, R(0)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 0
// CHECK-NEXT: R.advancedBy: 13, -3
// CHECK-NEXT: [10]

// Tests for advancing away from the end bound.
print("[\(advance(R(13), -99, R(16)).x)]")
// CHECK-NEXT: R.distanceTo: 13, 16
// CHECK-NEXT: R.advancedBy: 13, -99
// CHECK-NEXT: [-86]
print("[\(advance(R(16), 99, R(13)).x)]")
// CHECK-NEXT: R.distanceTo: 16, 13
// CHECK-NEXT: R.advancedBy: 16, 99
// CHECK-NEXT: [115]

print("all done.")
// CHECK-NEXT: all done.

