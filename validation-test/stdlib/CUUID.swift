// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// REQUIRES: OS=linux-gnu

import Swift
import StdlibUnittest


import CUUID

var CUUIDTestSuite = TestSuite("CUUID")

CUUIDTestSuite.test("uuid") {
  var uuid: uuid_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  withUnsafeMutablePointer(&uuid) {
    uuid_generate_random(unsafeBitCast($0, to: UnsafeMutablePointer<UInt8>.self))
  }
}

runAllTests()
