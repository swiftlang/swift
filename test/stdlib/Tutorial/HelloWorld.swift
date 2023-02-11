// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: tutorial
// UNSUPPORTED: freestanding

import StdlibUnittest
import Tutorial

let suite = TestSuite("HelloWorld")

if #available(SwiftStdlib 9999, *) {
  suite.test("Basic") {
    expectEqual(helloWorld(), "Hello World!")
  }
}

runAllTests()
