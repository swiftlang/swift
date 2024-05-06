// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-move-only -enable-builtin-module)

// REQUIRES: executable_test

import StdlibUnittest
import Builtin

class Klass {}

var suite = TestSuite("LifetimeManagement")

suite.test("_copy") {
  let k = Klass()
  expectTrue(k === _copy(k))
}

suite.test("copy") {
  let k = Klass()
  expectTrue(k === copy k)
}

suite.test("move") {
  let k = Klass()
  let k2 = k
  expectTrue(k2 === consume k)
}

runAllTests()
