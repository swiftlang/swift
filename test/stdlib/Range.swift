// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest

var RangeTestCase = TestCase("Range")

RangeTestCase.test("ReverseRange") {
  // We no longer have a ReverseRange, but we can still make sure that
  // lazy reversal works correctly.
  expectTrue(equal(lazy(0..<10).reverse(), [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]))
}

func isEquatable<E : Equatable>(e: E) {}

RangeTestCase.test("Range/Equatable") {
  let r1 = Range(start: 0, end: 0)
  let r2 = Range(start: 0, end: 1)
  isEquatable(r1)
  expectTrue(r1 == r1)
  expectFalse(r1 != r1)
  expectFalse(r1 == r2)
  expectTrue(r1 != r2)
}

RangeTestCase.run()
// CHECK: {{^}}Range: All tests passed

//===---
// Misc tests.
//===---

// CHECK: testing...
println("testing...")

for i in stride(from: 1.4, through: 3.4, by: 1) { println(i) }
// CHECK-NEXT: 1.4
// CHECK-NEXT: 2.4
// CHECK-NEXT: 3.4


// <rdar://problem/17054014> map method should exist on ranges
for i in ((1...3).map {$0*2}) {
  println(i)
}
// CHECK-NEXT: 2
// CHECK-NEXT: 4
// CHECK-NEXT: 6

// CHECK-NEXT: done.
println("done.")
