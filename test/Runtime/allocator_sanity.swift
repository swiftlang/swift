// RUN: %target-run-simple-swiftgyb

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var AllocationsTestSuite = TestSuite("Allocations")

AllocationsTestSuite.test("absurd.allocation.misaligned") {
  expectCrashLater()
  let mustFail = UnsafeMutableRawPointer.allocate(byteCount: 1024,
                                                  alignment: 19)
  expectUnreachable()
  _ = mustFail
}

AllocationsTestSuite.test("absurd.allocation.gargantuan") {
  expectCrashLater()
  let mustFail = UnsafeMutableRawPointer.allocate(byteCount: Int.max,
                                                  alignment: 0)
  expectUnreachable()
  _ = mustFail
}

runAllTests()
