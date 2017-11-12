// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: OS=linux-gnu

import StdlibUnittest
import CUUID

/// Make sure that the module map for the uuid.h header works
var CUUIDTestSuite = TestSuite("CUUID")

/// Generates a random UUID
CUUIDTestSuite.test("uuid") {
  var uuid: uuid_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  withUnsafeMutablePointer(to: &uuid) {
    uuid_generate_random(unsafeBitCast($0, to: UnsafeMutablePointer<UInt8>.self))
  }
}

runAllTests()
