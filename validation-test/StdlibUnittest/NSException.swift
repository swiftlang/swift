// RUN: %target-run-simple-swift 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// FIXME: this test is failing for watchos <rdar://problem/29997073>
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import ObjectiveC
import Foundation

// Don't actually exit with a non-zero status, just say we're going to do it.
_setTestSuiteFailedCallback() { print("abort()") }


func raiseNSException() {
  NSException(name: NSExceptionName(rawValue: "Trogdor"), reason: "Burnination", userInfo: nil).raise()
}

var TestSuiteCrashes = TestSuite("NSExceptionCrashes")

TestSuiteCrashes.test("uncaught") {
  print("uncaught")
  raiseNSException()
}
// CHECK-LABEL: stdout>>> uncaught
// CHECK: stderr>>> *** [StdlibUnittest] Terminating due to uncaught exception Trogdor: Burnination
// CHECK: stderr>>> CRASHED: SIG
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] NSExceptionCrashes.uncaught

TestSuiteCrashes.test("crashesAsExpected") {
  print("crashesAsExpected")
  expectCrashLater()
  raiseNSException()
}
// CHECK-LABEL: stdout>>> crashesAsExpected
// CHECK: stderr>>> *** [StdlibUnittest] Terminating due to uncaught exception Trogdor: Burnination
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] NSExceptionCrashes.crashesAsExpected

TestSuiteCrashes.test("crashesWithMessage")
  .crashOutputMatches("libUnittest]")
  .crashOutputMatches("Trogdor")
  .crashOutputMatches("Burnination").code {
  print("crashesWithMessage")
  expectCrashLater()
  raiseNSException()
}
// CHECK-LABEL: stdout>>> crashesWithMessage
// CHECK: stderr>>> *** [StdlibUnittest] Terminating due to uncaught exception Trogdor: Burnination
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] NSExceptionCrashes.crashesWithMessage

TestSuiteCrashes.test("nonNSException")
  .crashOutputMatches("countryside").code {
  print("nonNSException")
  expectCrashLater()
  objc_exception_throw("countryside")
}
// CHECK-LABEL: stdout>>> nonNSException
// CHECK: stderr>>> *** [StdlibUnittest] Terminating due to uncaught exception: countryside
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] NSExceptionCrashes.nonNSException

// CHECK: NSExceptionCrashes: Some tests failed, aborting
// CHECK: abort()

runAllTests()

