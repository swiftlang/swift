// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import struct_add_initializer


var StructAddInitializerTest = TestSuite("StructAddInitializer")

StructAddInitializerTest.test("AddInitializer") {
  let s = AddInitializer()

  expectEqual(0, s.x)
}

runAllTests()
