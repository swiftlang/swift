// RUN: %target-resilience-test
// REQUIRES: executable_test

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
