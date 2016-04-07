// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_add_property


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
    func increment(_ t: inout Int) {
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
