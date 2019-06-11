// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

// SR-10913
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import backward_deploy_top_level


var BackwardDeployTopLevelTest = TestSuite("BackwardDeployTopLevel")

BackwardDeployTopLevelTest.test("TopLevel") {
  if getVersion() == 1 {
    topLevelFunction(storedGlobal)
    topLevelFunction(computedGlobal)

    storedGlobal += 1
    computedGlobal += 1
  }
}

runAllTests()
