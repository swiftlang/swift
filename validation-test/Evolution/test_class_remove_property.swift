// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_remove_property


var ClassRemovePropertyTest = TestSuite("ClassRemoveProperty")

ClassRemovePropertyTest.test("RemoveStoredProperty") {
  let dog = RemoveStoredProperty(first: "Barnaby",
                                 middle: "Winston",
                                 last: "Fluffington")

  do {
    expectEqual(dog.name, "Barnaby Winston Fluffington")
  }
}

runAllTests()
