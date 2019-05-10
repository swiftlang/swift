// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import change_default_argument_to_magic


var ChangeDefaultArgumentToMagic = TestSuite("ChangeDefaultArgumentToMagic")

ChangeDefaultArgumentToMagic.test("ChangeDefaultArgumentToMagic") {
  changeDefaultArgumentToMagic()
}

runAllTests()
