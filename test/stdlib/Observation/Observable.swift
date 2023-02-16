// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: observation
// UNSUPPORTED: freestanding

import StdlibUnittest
import Observation

let suite = TestSuite("Observable")

if #available(SwiftStdlib 9999, *) {
  suite.test("Basic") {

  }
}

 runAllTests()