// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib

import StdlibUnittest

class Klass {}

var suite = TestSuite("LifetimeManagement")

suite.test("copy") {
  let k = Klass()
  expectTrue(k === _copy(k))
}

suite.test("move") {
  let k = Klass()
  let k2 = k
  expectTrue(k2 === _move(k))
}

runAllTests()
