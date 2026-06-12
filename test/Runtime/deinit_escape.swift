// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var DeinitEscapeTestSuite = TestSuite("DeinitEscape")

var globalObjects1: [AnyObject] = []
var globalObjects2: [AnyObject] = []

@inline(never)
func clearGlobalObjects1() {
  globalObjects1 = []
}

DeinitEscapeTestSuite.test("deinit escapes self") {
  expectCrashLater()

  class C {
    deinit {
      globalObjects2.append(self)
    }
  }
  do {
    let c = C()
    globalObjects1.append(c)
    _blackHole(c)
  }

  clearGlobalObjects1()

  expectUnreachable()
}

runAllTests()
