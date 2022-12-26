// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var DeinitEscapeTestSuite = TestSuite("DeinitEscape")

var globalObjects1: [AnyObject] = []
var globalObjects2: [AnyObject] = []

DeinitEscapeTestSuite.test("deinit escapes self") {
  expectCrashLater()

  class C {
    deinit {
      globalObjects2.append(self)
    }
  }
  globalObjects1.append(C())
  globalObjects1 = []

  expectUnreachable()
}

runAllTests()
