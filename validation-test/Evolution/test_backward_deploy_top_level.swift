// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

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
