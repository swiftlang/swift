// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_add_property


var ClassAddPropertyTest = TestSuite("ClassAddProperty")

ClassAddPropertyTest.test("AddStoredProperty") {
  let t1 = AddStoredProperty()
  let t2 = AddStoredProperty()

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

runAllTests()
