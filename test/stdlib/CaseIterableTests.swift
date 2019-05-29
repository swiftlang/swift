// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test

import StdlibUnittest

var CaseIterableTests = TestSuite("CaseIterableTests")

CaseIterableTests.test("Simple Enums") {
  enum SimpleEnum: CaseIterable {
    case bar
    case baz
    case quux
  }

  expectEqual(SimpleEnum.allCases.count, 3)
  expectTrue(SimpleEnum.allCases[0] == .bar)
  expectTrue(SimpleEnum.allCases[1] == .baz)
  expectTrue(SimpleEnum.allCases[2] == .quux)
}

runAllTests()
