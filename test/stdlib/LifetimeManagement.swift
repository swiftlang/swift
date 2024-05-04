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

// TODO: Remove.  Only exists to avoid a reverse condfail.
//       rdar://127516085 (Complete removal of Builtin.copy)
func _oldCopy<T>(_ value: T) -> T {
  #if $BuiltinCopy
    Builtin.copy(value)
  #else
    value
  #endif
}

suite.test("_oldCopy") {
  let k = Klass()
  expectTrue(k === _oldCopy(k))
}

