// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var DeinitEscapeTestSuite = TestSuite("DeinitEscape")

var globalObjects1: [AnyObject] = []
var globalObjects2: [AnyObject] = []

// Hide the array modifications in never-inline functions so the optimizer can't
// notice that they're dead stores.
@inline(never)
func clearGlobalObjects1() {
  globalObjects1 = []
}

@inline(never)
func escapeToGlobalObjects2(_ object: AnyObject) {
  globalObjects2.append(object)
}

DeinitEscapeTestSuite.test("deinit escapes self") {
  expectCrashLater()

  class C {
    deinit {
      escapeToGlobalObjects2(self)
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
