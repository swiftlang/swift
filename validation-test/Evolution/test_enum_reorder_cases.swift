// RUN: %target-resilience-test
// REQUIRES: executable_test

import enum_reorder_cases
import StdlibUnittest

var EnumReorderCases = TestSuite("EnumReorderCases")

func testGenericResilience<T: Equatable>(
  _ e: GenericResilientEnum<T>, expectedCase: Int, expectedValue: T
) {
  var whichCase = 0
  switch e {
    case .FirstCase:
      whichCase = 1
    case .SecondCase:
      whichCase = 2
    case .ThirdCase(let o):
      whichCase = 3
      expectEqual(o.someNumber(), 7)
    case .ForthCase(let t):
      whichCase = 4
      expectEqual(t, expectedValue)
  }
  expectEqual(whichCase, expectedCase)
}

EnumReorderCases.test("test1") {
  let e1 = GenericResilientEnum<Float>.FirstCase
  testGenericResilience(e1, expectedCase: 1, expectedValue: 0)
  expectEqual(getCase(e1), 1)
  let e2 = createGenericResilientEnum2()
  testGenericResilience(e2, expectedCase: 2, expectedValue: 0)
  let e3 = createGenericResilientEnum3()
  testGenericResilience(e3, expectedCase: 3, expectedValue: 0)
  let e4 = createGenericResilientEnum4(8)
  testGenericResilience(e4, expectedCase: 4, expectedValue: 8)
}

runAllTests()
