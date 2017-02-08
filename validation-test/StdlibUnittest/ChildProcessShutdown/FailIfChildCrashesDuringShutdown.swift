// RUN: %target-run-simple-swift 2>&1 | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest
#if os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Windows)
import Glibc
#else
import Darwin
#endif

_setTestSuiteFailedCallback() { print("abort()") }

//
// Test that harness aborts when a test crashes if a child process crashes
// after all tests have finished running.
//

var TestSuiteChildCrashes = TestSuite("TestSuiteChildCrashes")

TestSuiteChildCrashes.test("passes") {
  atexit {
    fatalError("crash at exit")
  }
}

// CHECK: [ RUN      ] TestSuiteChildCrashes.passes
// CHECK: [       OK ] TestSuiteChildCrashes.passes
// CHECK: TestSuiteChildCrashes: All tests passed
// CHECK: stderr>>> fatal error: crash at exit:
// CHECK: stderr>>> CRASHED: SIG
// CHECK: The child process failed during shutdown, aborting.
// CHECK: abort()

runAllTests()

