// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-move-only)

// REQUIRES: executable_test

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
  expectTrue(k2 === consume k)
}

runAllTests()
