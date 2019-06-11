// RUN: %target-resilience-test
// REQUIRES: executable_test

// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import struct_remove_property


var StructRemovePropertyTest = TestSuite("StructRemoveProperty")

StructRemovePropertyTest.test("RemoveStoredProperty") {
  var dog = RemoveStoredProperty(first: "Barnaby",
                                 middle: "Winston",
                                 last: "Fluffington")

  do {
    expectEqual(dog.name, "Barnaby Winston Fluffington")
  }
}

runAllTests()
