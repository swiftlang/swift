// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

enum Bewl : Boolean {
  case False, True

  var boolValue: Bool {
    switch self {
    case .False:
      return false
    case .True:
      return true
    }
  }
}

func truthy() -> Bewl {
  return .True
}

func falsy() -> Bewl {
  return .False
}

let LogicValueTests = TestSuite("LogicValue")
LogicValueTests.test("Basic") {
  // Logic values should convert to bool.
  struct X : Boolean {
    var boolValue: Bool { return false }
  }
  var anX = X()
  expectFalse(Bool(anX))

  expectFalse(!Bewl.True)
  expectTrue(!Bewl.False)

  // Test short-circuiting operators
  expectTrue(Bool(truthy() && truthy()))
  expectFalse(Bool(truthy() && falsy()))
  expectFalse(Bool(falsy() && truthy()))
  expectFalse(Bool(falsy() && falsy()))

  expectTrue(Bool(truthy() || truthy()))
  expectTrue(Bool(truthy() || falsy()))
  expectTrue(Bool(falsy() || truthy()))
  expectFalse(Bool(falsy() || falsy()))
}

runAllTests()
