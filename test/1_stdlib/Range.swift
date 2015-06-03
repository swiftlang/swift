// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest

var RangeTestSuite = TestSuite("Range")

RangeTestSuite.test("ReverseRange") {
  // We no longer have a ReverseRange, but we can still make sure that
  // lazy reversal works correctly.
  expectEqualSequence(lazy(0..<10).reverse(), [9, 8, 7, 6, 5, 4, 3, 2, 1, 0])
}

func isEquatable<E : Equatable>(e: E) {}

RangeTestSuite.test("Range/Equatable") {
  let r1 = Range(start: 0, end: 0)
  let r2 = Range(start: 0, end: 1)
  isEquatable(r1)
  expectTrue(r1 == r1)
  expectFalse(r1 != r1)
  expectFalse(r1 == r2)
  expectTrue(r1 != r2)
}

// Something to test with that distinguishes debugDescription from description
struct X<T : ForwardIndexType> : ForwardIndexType, CustomStringConvertible, CustomDebugStringConvertible {
  init(_ a: T) {
    self.a = a
  }

  func successor() -> X {
    return X(a.successor())
  }

  var description: String {
    return String(a)
  }

  var debugDescription: String {
    return "X(\(String(reflecting: a)))"
  }

  var a: T
}

func == <T : ForwardIndexType>(lhs: X<T>, rhs: X<T>) -> Bool {
  return lhs.a == rhs.a
}

RangeTestSuite.test("Printing") {
  expectEqual("0..<10", String(X(0)..<X(10)))
  expectEqual("Range(X(0)..<X(10))", String(reflecting: Range(X(0)..<X(10))))

  // No separate representation for closed Ranges yet
  expectEqual("10..<42", String(X(10)...X(41)))
  expectEqual("Range(X(10)..<X(42))", String(reflecting: Range(X(10)...X(41))))
}

RangeTestSuite.test("Pattern matching") {
  let x = 0..<20
  expectTrue(x ~= 10)
  expectFalse(x ~= 20)
  expectFalse(x ~= -1)

  // <rdar://21091371>.  Timeouts should make this test fail if this
  // ends up doing a linear search.
  expectFalse(1_000_000..<(1_000_000_000_000 as Int64) ~= 1)
}

RangeTestSuite.test("stride") {
  var result = [Double]()
  for i in stride(from: 1.4, through: 3.4, by: 1) {
    result.append(i)
  }
  expectEqual([ 1.4, 2.4, 3.4 ], result)
}

RangeTestSuite.test("map") {
  // <rdar://problem/17054014> map method should exist on ranges
  expectEqual([ 2, 4, 6 ], Array((1...3).map {$0*2}))
}

runAllTests()

