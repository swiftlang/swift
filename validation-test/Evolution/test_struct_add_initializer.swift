// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_add_initializer


var StructAddInitializerTest = TestSuite("StructAddInitializer")

StructAddInitializerTest.test("AddInitializer") {
  let s = AddInitializer()

  expectEqual(0, s.x)
}

runAllTests()
