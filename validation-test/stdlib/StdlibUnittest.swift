// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

import SwiftPrivate
import StdlibUnittest

_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
_setTestSuiteFailedCallback() { print("abort()") }

//
// Test that harness aborts when a test fails
//

var TestSuitePasses = TestSuite("TestSuitePasses")
TestSuitePasses.test("passes") {
  expectEqual(1, 1)
}
// CHECK: [       OK ] TestSuitePasses.passes{{$}}
// CHECK: TestSuitePasses: All tests passed

var TestSuiteUXPasses = TestSuite("TestSuiteUXPasses")
TestSuiteUXPasses.test("uxpasses").xfail(.OSXAny("")).code {
  expectEqual(1, 1)
}
// CHECK: [   UXPASS ] TestSuiteUXPasses.uxpasses{{$}}
// CHECK: TestSuiteUXPasses: Some tests failed, aborting
// CHECK: UXPASS: [uxpasses]
// CHECK: FAIL: []
// CHECK: SKIP: []
// CHECK: abort()

var TestSuiteFails = TestSuite("TestSuiteFails")
TestSuiteFails.test("fails") {
  expectEqual(1, 2)
}
// CHECK: [     FAIL ] TestSuiteFails.fails{{$}}
// CHECK: TestSuiteFails: Some tests failed, aborting
// CHECK: UXPASS: []
// CHECK: FAIL: [fails]
// CHECK: SKIP: []
// CHECK: abort()

var TestSuiteXFails = TestSuite("TestSuiteXFails")
TestSuiteXFails.test("xfails").xfail(.OSXAny("")).code {
  expectEqual(1, 2)
}
// CHECK: [    XFAIL ] TestSuiteXFails.xfails{{$}}
// CHECK: TestSuiteXFails: All tests passed

//
// Test 'xfail:' and 'skip:' annotations
//

var XFailsAndSkips = TestSuite("XFailsAndSkips")

// CHECK: [       OK ] XFailsAndSkips.passes{{$}}
XFailsAndSkips.test("passes") {
  expectEqual(1, 1)
}

// CHECK: [     FAIL ] XFailsAndSkips.fails{{$}}
XFailsAndSkips.test("fails") {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 passes{{$}}
XFailsAndSkips.test("xfail 10.9.3 passes")
  .xfail(.OSXBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 fails{{$}}
XFailsAndSkips.test("xfail 10.9.3 fails")
  .xfail(.OSXBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 passes{{$}}
XFailsAndSkips.test("skip 10.9.2 passes")
  .skip(.OSXBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 fails{{$}}
XFailsAndSkips.test("skip 10.9.2 fails")
  .skip(.OSXBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [ SKIP     ] XFailsAndSkips.skip 10.9.3 (skip: [OSX(10.9.3, reason: )]){{$}}
XFailsAndSkips.test("skip 10.9.3")
  .skip(.OSXBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
  fatalError("should not be executed")
}

// CHECK: XFailsAndSkips: Some tests failed, aborting
// CHECK: abort()

//
// Test custom XFAIL predicates
//

var XFailsCustomPredicates = TestSuite("XFailsCustomPredicates")

// CHECK: [    XFAIL ] XFailsCustomPredicates.matches{{$}}
XFailsCustomPredicates.test("matches")
  .xfail(.Custom({ true }, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsCustomPredicates.not matches{{$}}
XFailsCustomPredicates.test("not matches")
  .xfail(.Custom({ false }, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: XFailsCustomPredicates: All tests passed

//
// Test version comparison rules
//

var XFailsOSX = TestSuite("XFailsOSX")

// CHECK: [   UXPASS ] XFailsOSX.xfail OSX passes{{$}}
XFailsOSX.test("xfail OSX passes").xfail(.OSXAny("")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail OSX fails{{$}}
XFailsOSX.test("xfail OSX fails").xfail(.OSXAny("")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 9.*{{$}}
XFailsOSX.test("xfail 9.*").xfail(.OSXMajor(9, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.*{{$}}
XFailsOSX.test("xfail 10.*").xfail(.OSXMajor(10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.8{{$}}
XFailsOSX.test("xfail 10.8").xfail(.OSXMinor(10, 8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9{{$}}
XFailsOSX.test("xfail 10.9").xfail(.OSXMinor(10, 9, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.[7-8]{{$}}
XFailsOSX.test("xfail 10.[7-8]")
  .xfail(.OSXMinorRange(10, 7...8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.[9-10]{{$}}
XFailsOSX.test("xfail 10.[9-10]")
  .xfail(.OSXMinorRange(10, 9...10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.2{{$}}
XFailsOSX.test("xfail 10.9.2")
  .xfail(.OSXBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.3{{$}}
XFailsOSX.test("xfail 10.9.3")
  .xfail(.OSXBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.[1-2]{{$}}
XFailsOSX.test("xfail 10.9.[1-2]")
  .xfail(.OSXBugFixRange(10, 9, 1...2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.[3-4]{{$}}
XFailsOSX.test("xfail 10.9.[3-4]")
  .xfail(.OSXBugFixRange(10, 9, 3...4, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsOSX: Some tests failed, aborting
// CHECK: abort()

//
// Check that we pass through stdout and stderr
//

var PassThroughStdoutStderr = TestSuite("PassThroughStdoutStderr")

PassThroughStdoutStderr.test("hasNewline") {
  print("stdout first")
  print("stdout second")
  print("stdout third")

  var stderr = _Stderr()
  print("stderr first", &stderr)
  print("stderr second", &stderr)
  print("stderr third", &stderr)
}
// CHECK: [ RUN      ] PassThroughStdoutStderr.hasNewline
// CHECK-DAG: out>>> stdout first
// CHECK-DAG: out>>> stdout second
// CHECK-DAG: out>>> stdout third
// CHECK-DAG: err>>> stderr first
// CHECK-DAG: err>>> stderr second
// CHECK-DAG: err>>> stderr third
// CHECK: [       OK ] PassThroughStdoutStderr.hasNewline

PassThroughStdoutStderr.test("noNewline") {
  print("stdout first")
  print("stdout second")
  print("stdout third", appendNewline: false)

  var stderr = _Stderr()
  print("stderr first", &stderr)
  print("stderr second", &stderr)
  print("stderr third", &stderr, appendNewline: false)
}
// CHECK: [ RUN      ] PassThroughStdoutStderr.noNewline
// CHECK-DAG: out>>> stdout first
// CHECK-DAG: out>>> stdout second
// CHECK-DAG: out>>> stdout third
// CHECK-DAG: err>>> stderr first
// CHECK-DAG: err>>> stderr second
// CHECK-DAG: err>>> stderr third
// CHECK: [       OK ] PassThroughStdoutStderr.noNewline
// CHECK: PassThroughStdoutStderr: All tests passed

//
// Test 'setUp' and 'tearDown'
//

var TestSuiteWithSetUpPasses = TestSuite("TestSuiteWithSetUpPasses")

TestSuiteWithSetUpPasses.test("passes") {
  print("test body")
}

TestSuiteWithSetUpPasses.setUp {
  print("setUp")
}
// CHECK: [ RUN      ] TestSuiteWithSetUpPasses.passes
// CHECK: out>>> setUp
// CHECK: out>>> test body
// CHECK: [       OK ] TestSuiteWithSetUpPasses.passes
// CHECK: TestSuiteWithSetUpPasses: All tests passed

var TestSuiteWithSetUpFails = TestSuite("TestSuiteWithSetUpFails")

TestSuiteWithSetUpFails.test("fails") {
  print("test body")
}

TestSuiteWithSetUpFails.setUp {
  print("setUp")
  expectEqual(1, 2)
}
// CHECK: [ RUN      ] TestSuiteWithSetUpFails.fails
// CHECK: out>>> setUp
// CHECK: out>>> check failed at
// CHECK: out>>> test body
// CHECK: [     FAIL ] TestSuiteWithSetUpFails.fails
// CHECK: TestSuiteWithSetUpFails: Some tests failed, aborting

var TestSuiteWithTearDownPasses = TestSuite("TestSuiteWithTearDownPasses")

TestSuiteWithTearDownPasses.test("passes") {
  print("test body")
}

TestSuiteWithTearDownPasses.tearDown {
  print("tearDown")
}
// CHECK: [ RUN      ] TestSuiteWithTearDownPasses.passes
// CHECK: out>>> test body
// CHECK: out>>> tearDown
// CHECK: [       OK ] TestSuiteWithTearDownPasses.passes

var TestSuiteWithTearDownFails = TestSuite("TestSuiteWithTearDownFails")

TestSuiteWithTearDownFails.test("fails") {
  print("test body")
}

TestSuiteWithTearDownFails.tearDown {
  print("tearDown")
  expectEqual(1, 2)
}
// CHECK: TestSuiteWithTearDownPasses: All tests passed
// CHECK: [ RUN      ] TestSuiteWithTearDownFails.fails
// CHECK: out>>> test body
// CHECK: out>>> tearDown
// CHECK: out>>> check failed at
// CHECK: [     FAIL ] TestSuiteWithTearDownFails.fails
// CHECK: TestSuiteWithTearDownFails: Some tests failed, aborting

//
// Test assertions
//

var AssertionsTestSuite = TestSuite("Assertions")

AssertionsTestSuite.test("expectUnreachable") {
  expectUnreachable()
}
// CHECK: [ RUN      ] Assertions.expectUnreachable
// CHECK: out>>> check failed at
// CHECK: out>>> this code should not be executed
// CHECK: [     FAIL ] Assertions.expectUnreachable

AssertionsTestSuite.test("expectCrashLater/Pass") {
  let array: [Int] = _opaqueIdentity([])
  expectCrashLater()
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/Pass
// CHECK: err>>> CRASHED: SIG{{.*}}
// CHECK: [       OK ] Assertions.expectCrashLater/Pass

AssertionsTestSuite.test("expectCrashLater/UXPass")
  .xfail(.Custom({ true }, reason: "test"))
  .code {
  let array: [Int] = _opaqueIdentity([])
  expectCrashLater()
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/UXPass (XFAIL: [Custom(reason: test)])
// CHECK: err>>> CRASHED: SIG{{.*}}
// CHECK: [   UXPASS ] Assertions.expectCrashLater/UXPass

AssertionsTestSuite.test("expectCrashLater/Fail") {
  expectCrashLater()
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/Fail
// CHECK: expecting a crash, but the test did not crash
// CHECK: [     FAIL ] Assertions.expectCrashLater/Fail

AssertionsTestSuite.test("expectCrashLater/XFail")
  .xfail(.Custom({ true }, reason: "test"))
  .code {
  expectCrashLater()
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/XFail (XFAIL: [Custom(reason: test)])
// CHECK: expecting a crash, but the test did not crash
// CHECK: [    XFAIL ] Assertions.expectCrashLater/XFail

AssertionsTestSuite.test("UnexpectedCrash/RuntimeTrap") {
  let array: [Int] = _opaqueIdentity([])
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.UnexpectedCrash/RuntimeTrap
// CHECK: err>>> CRASHED: SIG{{.*}}
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.UnexpectedCrash/RuntimeTrap

AssertionsTestSuite.test("UnexpectedCrash/NullPointerDereference") {
  let ptr = _opaqueIdentity(UnsafePointer<Int>())
  _blackHole(ptr.memory)
}
// CHECK: [ RUN      ] Assertions.UnexpectedCrash/NullPointerDereference
// CHECK: err>>> CRASHED: SIG{{.*}}
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.UnexpectedCrash/NullPointerDereference

runAllTests()

