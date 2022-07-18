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
  // There is a chance that we'll actually be able to allocate Int.max bytes on
  // 32-bit platforms, since they often have 4GB address spaces and Int.max is
  // 2GB minus one byte. Allocate twice to ensure failure. That will (attempt
  // to) allocate 4GB minus two bytes, and we'll definitely have more than two
  // bytes of other stuff in the process.
  let mustFail = UnsafeMutableRawPointer.allocate(byteCount: Int.max,
                                                  alignment: 0)
  let mustFail2 = UnsafeMutableRawPointer.allocate(byteCount: Int.max,
                                                   alignment: 0)
  expectUnreachable()
  _ = mustFail
  _ = mustFail2
}

runAllTests()
