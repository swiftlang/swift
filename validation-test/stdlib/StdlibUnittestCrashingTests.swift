// RUN: %target-run-simple-swift 2>&1 | FileCheck %s

import StdlibUnittest

_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
_setTestSuiteFailedCallback() { print("abort()") }

//
// Test that harness aborts when a test crashes
//

var TestSuiteCrashes = TestSuite("TestSuiteCrashes")

TestSuiteCrashes.test("crashesUnexpectedly1") {
  print("crashesUnexpectedly1")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedly1
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [     FAIL ] TestSuiteCrashes.crashesUnexpectedly1

TestSuiteCrashes.test("passes1") {
  print("passes1")
  expectEqual(1, 1)
}
// CHECK: out>>> passes1
// CHECK: [       OK ] TestSuiteCrashes.passes1

TestSuiteCrashes.test("fails1") {
  print("fails1")
  expectEqual(1, 2)
}
// CHECK: out>>> fails1
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails1

TestSuiteCrashes.test("crashesUnexpectedly2") {
  print("crashesUnexpectedly2")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedly2
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [     FAIL ] TestSuiteCrashes.crashesUnexpectedly2

TestSuiteCrashes.test("passes2") {
  print("passes2")
  expectEqual(1, 1)
}
// CHECK: out>>> passes2
// CHECK: [       OK ] TestSuiteCrashes.passes2

TestSuiteCrashes.test("fails2") {
  print("fails2")
  expectEqual(1, 2)
}
// CHECK: out>>> fails2
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails2

TestSuiteCrashes.test("crashesAsExpected1") {
  print("crashesAsExpected1")
  expectCrashLater()
  fatalError("this should crash")
}
// CHECK: out>>> crashesAsExpected1
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [       OK ] TestSuiteCrashes.crashesAsExpected1

TestSuiteCrashes.test("passes3") {
  print("passes3")
  expectEqual(1, 1)
}
// CHECK: out>>> passes3
// CHECK: [       OK ] TestSuiteCrashes.passes3

TestSuiteCrashes.test("fails3") {
  print("fails3")
  expectEqual(1, 2)
}
// CHECK: out>>> fails3
// CHECK: out>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails3

TestSuiteCrashes.test("crashesUnexpectedlyXfail")
  .xfail(.OSXBugFix(10, 9, 3, reason: "")).code {
  print("crashesUnexpectedlyXfail")
  fatalError("this should crash")
}
// CHECK: out>>> crashesUnexpectedlyXfail
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [    XFAIL ] TestSuiteCrashes.crashesUnexpectedlyXfail

TestSuiteCrashes.test("crashesAsExpectedXfail")
  .xfail(.OSXBugFix(10, 9, 3, reason: "")).code {
  print("crashesAsExpectedXfail")
  expectCrashLater()
  fatalError("this should crash")
}
// CHECK: out>>> crashesAsExpectedXfail
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [   UXPASS ] TestSuiteCrashes.crashesAsExpectedXfail

TestSuiteCrashes.test("crashesWithMessagePasses")
  .crashOutputMatches("this should crash").code {
  print("abcd")
  expectCrashLater()
  fatalError("this should crash")
}
// CHECK: out>>> abcd
// CHECK: err>>> fatal error: this should crash:
// CHECK: err>>> CRASHED: SIG
// CHECK: [       OK ] TestSuiteCrashes.crashesWithMessagePasses

TestSuiteCrashes.test("crashesWithMessageFails")
  .crashOutputMatches("this should crash").code {
  print("this should crash")
  expectCrashLater()
  fatalError("unexpected message")
}
// CHECK: out>>> this should crash
// CHECK: err>>> fatal error: unexpected message:
// CHECK: err>>> CRASHED: SIG
// CHECK: did not find expected string after crash: "this should crash"
// CHECK: [     FAIL ] TestSuiteCrashes.crashesWithMessageFails

TestSuiteCrashes.test("crashesWithMultipleMessagesPasses")
  .crashOutputMatches("little dog")
  .crashOutputMatches("this should crash")
  .crashOutputMatches("too")
  .code {
  print("abcd")
  expectCrashLater()
  fatalError("this should crash and your little dog too")
}
// CHECK: out>>> abcd
// CHECK: err>>> fatal error: this should crash and your little dog too:
// CHECK: err>>> CRASHED: SIG
// CHECK: [       OK ] TestSuiteCrashes.crashesWithMultipleMessagesPasses

TestSuiteCrashes.test("crashesWithMultipleMessagesFails")
  .crashOutputMatches("unexpected message")
  .crashOutputMatches("this should crash")
  .crashOutputMatches("big dog")
  .crashOutputMatches("and your little dog too")
.code {
  print("this should crash")
  expectCrashLater()
  fatalError("unexpected message and your little dog too")
}
// CHECK: out>>> this should crash
// CHECK: err>>> fatal error: unexpected message and your little dog too:
// CHECK: err>>> CRASHED: SIG
// CHECK: did not find expected string after crash: "this should crash"
// CHECK: did not find expected string after crash: "big dog"
// CHECK: [     FAIL ] TestSuiteCrashes.crashesWithMultipleMessagesFails

// CHECK: TestSuiteCrashes: Some tests failed, aborting
// CHECK: abort()

runAllTests()

