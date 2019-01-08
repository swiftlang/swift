// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import backward_deploy_struct


var BackwardDeployStructTest = TestSuite("BackwardDeployStruct")

BackwardDeployStructTest.test("ResilientStruct") {
  if getVersion() == 1 {
    // FIXME: IRGen inserts the metadata load outside the version check
    // apparently. Work around that here. <rdar://problem/46438608>
    @inline(never) func helper() {
      var s = ResilientStruct()

      s.fn(s.storedProp)
      s.storedProp = 1
      s.storedProp += 1

      s.fn(s.computedProp)
      s.computedProp = 1
      s.computedProp += 1

      s.fn(s[0])
      s[0] = 1
      s[0] += 1
    }

    helper()
  }
}

BackwardDeployStructTest.test("FixedLayoutStruct") {
  if getVersion() == 1 {
    var s = FixedLayoutStruct()

    s.fn(s.storedProp)
    s.storedProp = 1
    s.storedProp += 1

    s.fn(s.computedProp)
    s.computedProp = 1
    s.computedProp += 1

    s.fn(s[0])
    s[0] = 1
    s[0] += 1
  }
}

runAllTests()
