// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_add_convenience_init

var AddConvenienceInitTest = TestSuite("AddConvenienceInit")

class Hobbit : AddConvenienceInit {
  public let name: String = "Bilbo"
}

AddConvenienceInitTest.test("AddConvenienceInit") {
  let t = Hobbit(z: 4)

  expectEqual("Bilbo", t.name)
  expectEqual(18, t.age)
}

runAllTests()
