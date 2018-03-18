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
  expectTrue(SimpleEnum.allCases.contains(.bar))
  expectTrue(SimpleEnum.allCases.contains(.baz))
  expectTrue(SimpleEnum.allCases.contains(.quux))
}

runAllTests()
