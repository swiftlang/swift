// RUN: %target-resilience-test
// REQUIRES: executable_test
// REQUIRES: no_asan

import StdlibUnittest
import class_add_property

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
