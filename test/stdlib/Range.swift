// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest

var RangeTestCase = TestCase("Range")

RangeTestCase.test("ReverseRange") {
  if true {
    let r = ReverseRange(range: 0..<0)
    expectTrue(r.isEmpty)
    expectEqual(0, r.bounds().0)
    expectEqual(0, r.bounds().1)
    checkSequence([], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(start: 0, pastEnd: 0)
    expectTrue(r.isEmpty)
    expectEqual(0, r.bounds().0)
    expectEqual(0, r.bounds().1)
    checkSequence([], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(range: 10..<10)
    expectTrue(r.isEmpty)
    expectEqual(10, r.bounds().0)
    expectEqual(10, r.bounds().1)
    checkSequence([], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(range: 0..<4)
    expectFalse(r.isEmpty)
    expectEqual(0, r.bounds().0)
    expectEqual(4, r.bounds().1)
    checkSequence([ 3, 2, 1, 0 ], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(start: 0, pastEnd: 4)
    expectFalse(r.isEmpty)
    expectEqual(0, r.bounds().0)
    expectEqual(4, r.bounds().1)
    checkSequence([ 3, 2, 1, 0 ], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(range: 10..<11)
    expectFalse(r.isEmpty)
    expectEqual(10, r.bounds().0)
    expectEqual(11, r.bounds().1)
    checkSequence([ 10 ], r, SourceLocStack().withCurrentLoc())
  }
  if true {
    let r = ReverseRange(range: -11..<(-10))
    expectFalse(r.isEmpty)
    expectEqual(-11, r.bounds().0)
    expectEqual(-10, r.bounds().1)
    checkSequence([ -11 ], r, SourceLocStack().withCurrentLoc())
  }
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

var x: Range<Int> = 3..<10

// CHECK-NEXT: 3579.
for a in x.by(2) {
  print(a)
}
println(".")

for i in 1.4...3.4 { println(i) }
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
