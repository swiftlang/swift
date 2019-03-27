// RUN: %target-resilience-test --backward-deployment
// REQUIRES: executable_test

// Uses swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import backward_deploy_always_emit_into_client


var BackwardDeployTopLevelTest = TestSuite("BackwardDeployAlwaysEmitIntoClient")

BackwardDeployTopLevelTest.test("BackwardDeployAlwaysEmitIntoClient") {
  expectEqual(serializedFunction(), getVersion() == 1 ? "new" : "old")
}

runAllTests()
