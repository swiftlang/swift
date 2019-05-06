// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import opaque_archetypes_change_underlying_type
import StdlibUnittest

var OpaqueArchetypes = TestSuite("OpaqueArchetypes")

OpaqueArchetypes.test("test1") {
  let o = resilientFunction()
  expectEqual(o.getValue(), expectedResult())
  expectEqual(MemoryLayout.size(ofValue: o), expectedSize())

  let c = Container()
  expectEqual(c.property.getValue(), c.expectedResult())
  expectEqual(MemoryLayout.size(ofValue: c.property), c.expectedSize())
}

runAllTests()
