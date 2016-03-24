// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_remove_property

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
