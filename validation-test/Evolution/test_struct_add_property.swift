// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_add_property

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StructAddPropertyTest = TestSuite("StructAddProperty")

StructAddPropertyTest.test("AddStoredProperty") {
  var t1 = AddStoredProperty()
  var t2 = AddStoredProperty()

  do {
    expectEqual(t1.forth, "Chuck Moore")
    expectEqual(t2.forth, "Chuck Moore")
  }

  do {
    t1.forth = "Elizabeth Rather"
    expectEqual(t1.forth, "Elizabeth Rather")
    expectEqual(t2.forth, "Chuck Moore")
  }


  do {
    if getVersion() > 0 {
      expectEqual(t1.languageDesigners, ["Elizabeth Rather",
                                         "John McCarthy",
                                         "Dennis Ritchie"])
      expectEqual(t2.languageDesigners, ["Chuck Moore",
                                         "John McCarthy",
                                         "Dennis Ritchie"])
    } else {
      expectEqual(t1.languageDesigners, ["Elizabeth Rather"])
      expectEqual(t2.languageDesigners, ["Chuck Moore"])
    }
  }
}

StructAddPropertyTest.test("ChangeEmptyToNonEmpty") {
  var t = ChangeEmptyToNonEmpty()

  do {
    expectEqual(t.property, 0)
    t.property += 1
    expectEqual(t.property, 1)
  }

  do {
    func increment(inout t: Int) {
      t += 1
    }

    increment(&t.property)
    expectEqual(t.property, 2)
  }

  do {
    expectEqual(getProperty(t), 2)
  }
}

runAllTests()
