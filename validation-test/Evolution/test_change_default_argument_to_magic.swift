// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import change_default_argument_to_magic


var ChangeDefaultArgumentToMagic = TestSuite("ChangeDefaultArgumentToMagic")

ChangeDefaultArgumentToMagic.test("ChangeDefaultArgumentToMagic") {
  changeDefaultArgumentToMagic()
}

runAllTests()
