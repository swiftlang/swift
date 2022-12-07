// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var DeinitEscapeTestSuite = TestSuite("DeinitEscape")

var globalObjects: [AnyObject] = []

DeinitEscapeTestSuite.test("deinit escapes self") {
  expectCrashLater()

  class C {
    deinit {
      globalObjects.append(self)
    }
  }
  _ = C()

  expectUnreachable()
}

runAllTests()
