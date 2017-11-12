// RUN: %target-run-simple-swift 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// FIXME: this test is failing for watchos <rdar://problem/29997033>
// UNSUPPORTED: OS=watchos

import StdlibUnittest


_setOverrideOSVersion(.osx(major: 10, minor: 9, bugFix: 3))
_setTestSuiteFailedCallback() { print("abort()") }

//
// Test that harness aborts when a test crashes during a test run.
//

var TestSuiteCrashes = TestSuite("TestSuiteCrashes")

TestSuiteCrashes.test("crashesUnexpectedly1") {
  print("crashesUnexpectedly1")
  fatalError("This should crash")
}
// CHECK: stdout>>> crashesUnexpectedly1
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> CRASHED: SIG
// CHECK: [     FAIL ] TestSuiteCrashes.crashesUnexpectedly1

TestSuiteCrashes.test("passes1") {
  print("passes1")
  expectEqual(1, 1)
}
// CHECK: stdout>>> passes1
// CHECK: [       OK ] TestSuiteCrashes.passes1

TestSuiteCrashes.test("fails1") {
  print("fails1")
  expectEqual(1, 2)
}
// CHECK: stdout>>> fails1
// CHECK: stdout>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails1

TestSuiteCrashes.test("crashesUnexpectedly2") {
  print("crashesUnexpectedly2")
  fatalError("This should crash")
}
// CHECK: stdout>>> crashesUnexpectedly2
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> CRASHED: SIG
// CHECK: [     FAIL ] TestSuiteCrashes.crashesUnexpectedly2

TestSuiteCrashes.test("passes2") {
  print("passes2")
  expectEqual(1, 1)
}
// CHECK: stdout>>> passes2
// CHECK: [       OK ] TestSuiteCrashes.passes2

TestSuiteCrashes.test("fails2") {
  print("fails2")
  expectEqual(1, 2)
}
// CHECK: stdout>>> fails2
// CHECK: stdout>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails2

TestSuiteCrashes.test("crashesAsExpected1") {
  print("crashesAsExpected1")
  expectCrashLater()
  fatalError("This should crash")
}
// CHECK: stdout>>> crashesAsExpected1
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] TestSuiteCrashes.crashesAsExpected1

TestSuiteCrashes.test("passes3") {
  print("passes3")
  expectEqual(1, 1)
}
// CHECK: stdout>>> passes3
// CHECK: [       OK ] TestSuiteCrashes.passes3

TestSuiteCrashes.test("fails3") {
  print("fails3")
  expectEqual(1, 2)
}
// CHECK: stdout>>> fails3
// CHECK: stdout>>> check failed
// CHECK: [     FAIL ] TestSuiteCrashes.fails3

TestSuiteCrashes.test("crashesUnexpectedlyXfail")
  .xfail(.osxBugFix(10, 9, 3, reason: "")).code {
  print("crashesUnexpectedlyXfail")
  fatalError("This should crash")
}
// CHECK: stdout>>> crashesUnexpectedlyXfail
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> CRASHED: SIG
// CHECK: [    XFAIL ] TestSuiteCrashes.crashesUnexpectedlyXfail

TestSuiteCrashes.test("crashesAsExpectedXfail")
  .xfail(.osxBugFix(10, 9, 3, reason: "")).code {
  print("crashesAsExpectedXfail")
  expectCrashLater()
  fatalError("This should crash")
}
// CHECK: stdout>>> crashesAsExpectedXfail
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [   UXPASS ] TestSuiteCrashes.crashesAsExpectedXfail

TestSuiteCrashes.test("crashesWithMessagePasses")
  .crashOutputMatches("This should crash").code {
  print("abcd")
  expectCrashLater()
  fatalError("This should crash")
}
// CHECK: stdout>>> abcd
// CHECK: stderr>>> Fatal error: This should crash:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] TestSuiteCrashes.crashesWithMessagePasses

TestSuiteCrashes.test("crashesWithMessageFails")
  .crashOutputMatches("This should crash").code {
  print("This should crash")
  expectCrashLater()
  fatalError("unexpected message")
}
// CHECK: stdout>>> This should crash
// CHECK: stderr>>> Fatal error: unexpected message:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: did not find expected string after crash: "This should crash"
// CHECK: [     FAIL ] TestSuiteCrashes.crashesWithMessageFails

TestSuiteCrashes.test("crashesWithMultipleMessagesPasses")
  .crashOutputMatches("little dog")
  .crashOutputMatches("This should crash")
  .crashOutputMatches("too")
  .code {
  print("abcd")
  expectCrashLater()
  fatalError("This should crash and your little dog too")
}
// CHECK: stdout>>> abcd
// CHECK: stderr>>> Fatal error: This should crash and your little dog too:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: [       OK ] TestSuiteCrashes.crashesWithMultipleMessagesPasses

TestSuiteCrashes.test("crashesWithMultipleMessagesFails")
  .crashOutputMatches("unexpected message")
  .crashOutputMatches("This should crash")
  .crashOutputMatches("big dog")
  .crashOutputMatches("and your little dog too")
.code {
  print("This should crash")
  expectCrashLater()
  fatalError("unexpected message and your little dog too")
}
// CHECK: stdout>>> This should crash
// CHECK: stderr>>> Fatal error: unexpected message and your little dog too:
// CHECK: stderr>>> OK: saw expected "crashed: sig
// CHECK: did not find expected string after crash: "This should crash"
// CHECK: did not find expected string after crash: "big dog"
// CHECK: [     FAIL ] TestSuiteCrashes.crashesWithMultipleMessagesFails

// CHECK: TestSuiteCrashes: Some tests failed, aborting
// CHECK: abort()

runAllTests()

