// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

class Klass {}

var suite = TestSuite("LifetimeManagement")

suite.test("copy") {
  let k = Klass()
  expectTrue(k === _copy(k))
}

runAllTests()