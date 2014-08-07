// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out 2>&1 | FileCheck %s

import StdlibUnittest

_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
_setTestCaseFailedCallback() { println("abort()") }

//
// Test that harness aborts when a test crashes
//

var TestCaseCrashes = TestCase("TestCaseCrashes")

TestCaseCrashes.test("crashesUnexpectedly1") {
  println("crashesUnexpectedly1")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedly1
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [     FAIL ] TestCaseCrashes.crashesUnexpectedly1

TestCaseCrashes.test("passes1") {
  println("passes1")
  expectEqual(1, 1)
}
// CHECK: out>>> passes1
// CHECK: [       OK ] TestCaseCrashes.passes1

TestCaseCrashes.test("fails1") {
  println("fails1")
  expectEqual(1, 2)
}
// CHECK: out>>> fails1
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestCaseCrashes.fails1

TestCaseCrashes.test("crashesUnexpectedly2") {
  println("crashesUnexpectedly2")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedly2
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [     FAIL ] TestCaseCrashes.crashesUnexpectedly2

TestCaseCrashes.test("passes2") {
  println("passes2")
  expectEqual(1, 1)
}
// CHECK: out>>> passes2
// CHECK: [       OK ] TestCaseCrashes.passes2

TestCaseCrashes.test("fails2") {
  println("fails2")
  expectEqual(1, 2)
}
// CHECK: out>>> fails2
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestCaseCrashes.fails2

TestCaseCrashes.test("crashesAsExpected1") {
  println("crashesAsExpected1")
  expectCrashLater()
  fatalError("this should crash")
}
// CHECK: out>>> crashesAsExpected1
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [       OK ] TestCaseCrashes.crashesAsExpected1

TestCaseCrashes.test("passes3") {
  println("passes3")
  expectEqual(1, 1)
}
// CHECK: out>>> passes3
// CHECK: [       OK ] TestCaseCrashes.passes3

TestCaseCrashes.test("fails3") {
  println("fails3")
  expectEqual(1, 2)
}
// CHECK: out>>> fails3
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestCaseCrashes.fails3

TestCaseCrashes.testXFail("crashesUnexpectedlyXfail", xfail: [.OSXBugFix(10, 9, 3, reason: "")]) {
  println("crashesUnexpectedlyXfail")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedlyXfail
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [    XFAIL ] TestCaseCrashes.crashesUnexpectedlyXfail

TestCaseCrashes.testXFail("crashesAsExpectedXfail", xfail: [.OSXBugFix(10, 9, 3, reason: "")]) {
  println("crashesAsExpectedXfail")
  expectCrashLater()
  fatalError("this should crash")
}
// CHECK: out>>> crashesAsExpectedXfail
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [   UXPASS ] TestCaseCrashes.crashesAsExpectedXfail

// CHECK: TestCaseCrashes: Some tests failed, aborting
// CHECK: abort()

runAllTests()

