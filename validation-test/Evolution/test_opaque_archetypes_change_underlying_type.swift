// RUN: %target-resilience-test
// REQUIRES: executable_test

import opaque_archetypes_change_underlying_type
import StdlibUnittest

var OpaqueArchetypes = TestSuite("OpaqueArchetypes")

OpaqueArchetypes.test("test1") {
  if #available(macOS 15.0, iOS 13.0, watchOS 6.0, tvOS 13.0, *) {
    let o = resilientFunction()
    expectEqual(o.getValue(), expectedResult())
    expectEqual(MemoryLayout.size(ofValue: o), expectedSize())

    let c = Container()
    expectEqual(c.property.getValue(), c.expectedResult())
    expectEqual(MemoryLayout.size(ofValue: c.property), c.expectedSize())
  }
}

runAllTests()
